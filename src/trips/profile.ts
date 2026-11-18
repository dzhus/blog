import { pathLengthMeters } from "./gpx.ts";
import { findTimeZone } from "./timezone.ts";
import type { TimedLatLon, TripProfile, TripTrack } from "./types.ts";

/** Max profile vertices retained per UTC calendar day after downsampling. */
export const PROFILE_POINTS_PER_DAY = 700;

type ProfileSample = {
  trackIdx: number;
  distanceKm: number;
  elevM: number;
  lat: number;
  lon: number;
  day: string;
  timeMs: number | null;
};

function calendarDateFromMs(ms: number): string {
  return new Date(ms).toISOString().slice(0, 10);
}

function haversineKm(
  a: { lat: number; lon: number },
  b: { lat: number; lon: number },
): number {
  return pathLengthMeters([a, b]) / 1000;
}

/**
 * Downsample one calendar day's samples to at most `maxPts`, keeping ends and
 * local elevation extrema when possible.
 */
export function downsampleDaySamples<T extends { elevM: number }>(
  samples: T[],
  maxPts: number,
): T[] {
  const n = samples.length;
  if (n <= maxPts) return samples;
  if (maxPts < 2) return samples.slice(0, maxPts);

  const keep = new Set<number>([0, n - 1]);
  for (let i = 1; i < n - 1; i++) {
    const e = samples[i]!.elevM;
    const prev = samples[i - 1]!.elevM;
    const next = samples[i + 1]!.elevM;
    if ((e >= prev && e >= next) || (e <= prev && e <= next)) {
      keep.add(i);
    }
  }
  for (let k = 0; k < maxPts; k++) {
    keep.add(Math.round((k / (maxPts - 1)) * (n - 1)));
  }

  let indices = [...keep].sort((a, b) => a - b);
  if (indices.length > maxPts) {
    const inner = indices.slice(1, -1);
    const budget = maxPts - 2;
    const thinned = [indices[0]!];
    if (budget > 0 && inner.length > 0) {
      if (budget === 1) {
        thinned.push(inner[Math.floor(inner.length / 2)]!);
      } else {
        for (let k = 0; k < budget; k++) {
          thinned.push(
            inner[Math.round((k / (budget - 1)) * (inner.length - 1))]!,
          );
        }
      }
    }
    thinned.push(indices[indices.length - 1]!);
    indices = [...new Set(thinned)].sort((a, b) => a - b);
  }

  return indices.map((i) => samples[i]!);
}

/**
 * Build a concatenated trip elevation profile from DEM-sampled track points.
 * Downsamples per calendar day (not per track).
 */
export function buildTripProfile(
  tracks: TripTrack[],
  trackPointLists: TimedLatLon[][],
  elevationLists: number[][],
): TripProfile | null {
  if (tracks.length === 0 || trackPointLists.length !== tracks.length) {
    return null;
  }

  const samples: ProfileSample[] = [];
  let distanceOffsetKm = 0;

  for (let trackIdx = 0; trackIdx < tracks.length; trackIdx++) {
    const points = trackPointLists[trackIdx]!;
    const elevations = elevationLists[trackIdx]!;
    if (points.length === 0 || elevations.length !== points.length) continue;

    let dayFallback: string | null = null;
    for (const p of points) {
      if (p.timeMs != null) {
        dayFallback = calendarDateFromMs(p.timeMs);
        break;
      }
    }

    let distAlongKm = 0;
    for (let i = 0; i < points.length; i++) {
      const p = points[i]!;
      if (i > 0) {
        distAlongKm += haversineKm(points[i - 1]!, p);
      }
      const day =
        p.timeMs != null
          ? calendarDateFromMs(p.timeMs)
          : (dayFallback ?? "unknown");
      if (p.timeMs != null) dayFallback = day;
      samples.push({
        trackIdx,
        distanceKm: distanceOffsetKm + distAlongKm,
        elevM: elevations[i]!,
        lat: p.lat,
        lon: p.lon,
        day,
        timeMs: p.timeMs,
      });
    }
    distanceOffsetKm += distAlongKm;
  }

  if (samples.length < 2) return null;

  let startMs: number | null = null;
  for (const s of samples) {
    if (s.timeMs != null) {
      startMs = s.timeMs;
      break;
    }
  }

  const dayOrder: string[] = [];
  const byDay = new Map<string, ProfileSample[]>();
  for (const s of samples) {
    let list = byDay.get(s.day);
    if (!list) {
      list = [];
      byDay.set(s.day, list);
      dayOrder.push(s.day);
    }
    list.push(s);
  }

  const selected: ProfileSample[] = [];
  for (const day of dayOrder) {
    selected.push(
      ...downsampleDaySamples(byDay.get(day)!, PROFILE_POINTS_PER_DAY),
    );
  }
  selected.sort((a, b) => a.distanceKm - b.distanceKm || a.trackIdx - b.trackIdx);

  const dayIndexByDate = new Map<string, number>();
  dayOrder.forEach((d, i) => dayIndexByDate.set(d, i));

  const timed = startMs != null;
  const trackPoints: number[][][] = tracks.map(() => []);
  let lastTimeMs = startMs;
  for (const s of selected) {
    const point: number[] = [
      round(s.distanceKm, 4),
      round(s.elevM, 1),
      round(s.lat, 6),
      round(s.lon, 6),
      dayIndexByDate.get(s.day)!,
    ];
    if (timed) {
      if (s.timeMs != null) lastTimeMs = s.timeMs;
      point.push(Math.round((lastTimeMs! - startMs!) / 1000));
    }
    trackPoints[s.trackIdx]!.push(point);
  }

  const days = dayOrder.map((date) => {
    const daySamples = selected.filter((s) => s.day === date);
    const startKm = daySamples[0]!.distanceKm;
    const endKm = daySamples[daySamples.length - 1]!.distanceKm;
    const day: TripProfile["days"][number] = {
      date,
      startKm: round(startKm, 4),
      endKm: round(endKm, 4),
    };
    if (timed) {
      const timedSamples = daySamples.filter((s) => s.timeMs != null);
      if (timedSamples.length > 0) {
        day.startElapsedSec = Math.round(
          (timedSamples[0]!.timeMs! - startMs!) / 1000,
        );
        day.endElapsedSec = Math.round(
          (timedSamples[timedSamples.length - 1]!.timeMs! - startMs!) / 1000,
        );
      }
    }
    return day;
  });

  const profile: TripProfile = {
    tracks: tracks.map((t, i) => ({
      id: t.id,
      color: t.color,
      points: trackPoints[i]!,
    })),
    days,
  };
  if (timed) {
    profile.startMs = startMs!;
    const anchor = samples.find((s) => s.timeMs != null);
    if (anchor) {
      const timeZone = findTimeZone(anchor.lat, anchor.lon);
      if (timeZone) profile.timeZone = timeZone;
    }
  }
  return profile;
}

function round(n: number, digits: number): number {
  const f = 10 ** digits;
  return Math.round(n * f) / f;
}
