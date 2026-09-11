/** Distinct colours for GPX overlays on a greyscale basemap. */
const TRACK_COLORS = [
  "#c41e3a",
  "#2060a0",
  "#d48000",
  "#2a7a2a",
  "#7b2d8e",
  "#008080",
  "#b85c38",
  "#3d5a80",
];

export function trackColor(index: number): string {
  return TRACK_COLORS[index % TRACK_COLORS.length]!;
}
