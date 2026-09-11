/** Distinct colours for GPX overlays on a greyscale basemap.
 * Order: blue → green → purple → remaining accents. */
const TRACK_COLORS = [
  "#2060a0", // blue
  "#2a7a2a", // green
  "#7b2d8e", // purple
  "#008080", // teal
  "#b85c38", // brown
  "#3d5a80", // slate
  "#c41e3a", // red
  "#d48000", // orange
];

export function trackColor(index: number): string {
  return TRACK_COLORS[index % TRACK_COLORS.length]!;
}
