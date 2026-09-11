import fs from "node:fs";
import path from "node:path";
import sharp from "sharp";

export type ImageDerivatives = {
  gridThumbRel: string;
  mapThumbRel: string;
  displayRel: string;
  originalRel: string;
};

const GRID = { max: 600, quality: 80 } as const;
const MAP = { size: 48, quality: 75 } as const;
const DISPLAY = { max: 2400, quality: 92 } as const;

function sourceKey(srcPath: string): string {
  const st = fs.statSync(srcPath);
  return `${st.mtimeMs}_${st.size}`;
}

async function writeIfNeeded(
  destPath: string,
  metaPath: string,
  key: string,
  build: () => Promise<void>,
): Promise<void> {
  fs.mkdirSync(path.dirname(destPath), { recursive: true });
  if (
    fs.existsSync(destPath) &&
    fs.existsSync(metaPath) &&
    fs.readFileSync(metaPath, "utf8") === key
  ) {
    return;
  }
  await build();
  fs.writeFileSync(metaPath, key);
}

export async function processPhotoImages(
  srcPath: string,
  slug: string,
  filename: string,
  cacheTripDir: string,
  siteTripDir: string,
): Promise<ImageDerivatives> {
  const src = sourceKey(srcPath);
  const gridKey = `${src}_grid_${GRID.max}_q${GRID.quality}`;
  const mapKey = `${src}_map_${MAP.size}_q${MAP.quality}`;
  const displayKey = `${src}_display_${DISPLAY.max}_q${DISPLAY.quality}`;

  const base = path.basename(filename);
  const stem = path.basename(base, path.extname(base));
  const outName = `${stem}.jpg`;

  const cacheGrid = path.join(cacheTripDir, "grid", outName);
  const cacheMap = path.join(cacheTripDir, "map", outName);
  const cacheDisplay = path.join(cacheTripDir, "display", outName);
  const metaGrid = path.join(cacheTripDir, "grid", `${outName}.key`);
  const metaMap = path.join(cacheTripDir, "map", `${outName}.key`);
  const metaDisplay = path.join(cacheTripDir, "display", `${outName}.key`);

  const needsGrid =
    !(
      fs.existsSync(cacheGrid) &&
      fs.existsSync(metaGrid) &&
      fs.readFileSync(metaGrid, "utf8") === gridKey
    );
  const needsMap =
    !(
      fs.existsSync(cacheMap) &&
      fs.existsSync(metaMap) &&
      fs.readFileSync(metaMap, "utf8") === mapKey
    );
  const needsDisplay =
    !(
      fs.existsSync(cacheDisplay) &&
      fs.existsSync(metaDisplay) &&
      fs.readFileSync(metaDisplay, "utf8") === displayKey
    );

  const input =
    needsGrid || needsMap || needsDisplay
      ? await fs.promises.readFile(srcPath)
      : null;

  if (needsGrid) {
    if (!input) throw new Error(`Missing image buffer for ${srcPath}`);
    await writeIfNeeded(cacheGrid, metaGrid, gridKey, async () => {
      await sharp(input, { failOn: "none" })
        .rotate()
        .resize({
          width: GRID.max,
          height: GRID.max,
          fit: "inside",
          withoutEnlargement: true,
        })
        .jpeg({ quality: GRID.quality, mozjpeg: true })
        .toFile(cacheGrid);
    });
  }

  if (needsMap) {
    if (!input) throw new Error(`Missing image buffer for ${srcPath}`);
    await writeIfNeeded(cacheMap, metaMap, mapKey, async () => {
      await sharp(input, { failOn: "none" })
        .rotate()
        .resize({ width: MAP.size, height: MAP.size, fit: "cover" })
        .jpeg({ quality: MAP.quality, mozjpeg: true })
        .toFile(cacheMap);
    });
  }

  if (needsDisplay) {
    if (!input) throw new Error(`Missing image buffer for ${srcPath}`);
    await writeIfNeeded(cacheDisplay, metaDisplay, displayKey, async () => {
      await sharp(input, { failOn: "none" })
        .rotate()
        .resize({
          width: DISPLAY.max,
          height: DISPLAY.max,
          fit: "inside",
          withoutEnlargement: true,
        })
        .jpeg({ quality: DISPLAY.quality, mozjpeg: true })
        .toFile(cacheDisplay);
    });
  }

  const siteGrid = path.join(siteTripDir, "thumbs", "grid", outName);
  const siteMap = path.join(siteTripDir, "thumbs", "map", outName);
  const siteDisplay = path.join(siteTripDir, "display", outName);
  const siteOriginal = path.join(siteTripDir, "originals", base);

  for (const [from, to] of [
    [cacheGrid, siteGrid],
    [cacheMap, siteMap],
    [cacheDisplay, siteDisplay],
  ] as const) {
    fs.mkdirSync(path.dirname(to), { recursive: true });
    fs.copyFileSync(from, to);
  }

  fs.mkdirSync(path.dirname(siteOriginal), { recursive: true });
  fs.copyFileSync(srcPath, siteOriginal);

  return {
    gridThumbRel: `/trips/${slug}/thumbs/grid/${outName}`,
    mapThumbRel: `/trips/${slug}/thumbs/map/${outName}`,
    displayRel: `/trips/${slug}/display/${outName}`,
    originalRel: `/trips/${slug}/originals/${base}`,
  };
}
