import fs from "node:fs";
import path from "node:path";
import sharp from "sharp";

export type ImageDerivatives = {
  gridThumbRel: string;
  mapThumbRel: string;
  displayRel: string;
  originalRel: string;
};

function cacheKey(srcPath: string): string {
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
  const key = cacheKey(srcPath);
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
      fs.readFileSync(metaGrid, "utf8") === key
    );
  const needsMap =
    !(
      fs.existsSync(cacheMap) &&
      fs.existsSync(metaMap) &&
      fs.readFileSync(metaMap, "utf8") === key
    );
  const needsDisplay =
    !(
      fs.existsSync(cacheDisplay) &&
      fs.existsSync(metaDisplay) &&
      fs.readFileSync(metaDisplay, "utf8") === key
    );

  // Read once into memory so sharp never holds path-based FileHandles.
  const input =
    needsGrid || needsMap || needsDisplay
      ? await fs.promises.readFile(srcPath)
      : null;

  if (needsGrid) {
    if (!input) throw new Error(`Missing image buffer for ${srcPath}`);
    await writeIfNeeded(cacheGrid, metaGrid, key, async () => {
      await sharp(input, { failOn: "none" })
        .rotate()
        .resize({
          width: 600,
          height: 600,
          fit: "inside",
          withoutEnlargement: true,
        })
        .jpeg({ quality: 80, mozjpeg: true })
        .toFile(cacheGrid);
    });
  }

  if (needsMap) {
    if (!input) throw new Error(`Missing image buffer for ${srcPath}`);
    await writeIfNeeded(cacheMap, metaMap, key, async () => {
      await sharp(input, { failOn: "none" })
        .rotate()
        .resize({ width: 48, height: 48, fit: "cover" })
        .jpeg({ quality: 75, mozjpeg: true })
        .toFile(cacheMap);
    });
  }

  if (needsDisplay) {
    if (!input) throw new Error(`Missing image buffer for ${srcPath}`);
    await writeIfNeeded(cacheDisplay, metaDisplay, key, async () => {
      await sharp(input, { failOn: "none" })
        .rotate()
        .resize({
          width: 2048,
          height: 2048,
          fit: "inside",
          withoutEnlargement: true,
        })
        .jpeg({ quality: 85, mozjpeg: true })
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
