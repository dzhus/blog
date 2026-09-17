import fs from "node:fs";
import path from "node:path";
import sharp from "sharp";
import { sourceKey } from "./exif.ts";

export type ImageDerivatives = {
  gridThumbRel: string;
  mapThumbRel: string;
  displayRel: string;
  originalRel: string;
};

const DERIVATIVES = [
  {
    sub: "grid",
    siteSub: path.join("thumbs", "grid"),
    keySuffix: "grid_600_q80",
    resize: { width: 600, height: 600, fit: "inside" as const, withoutEnlargement: true },
    quality: 80,
  },
  {
    sub: "map",
    siteSub: path.join("thumbs", "map"),
    keySuffix: "map_48_q75",
    resize: { width: 48, height: 48, fit: "cover" as const },
    quality: 75,
  },
  {
    sub: "display",
    siteSub: "display",
    keySuffix: "display_2400_q92",
    resize: { width: 2400, height: 2400, fit: "inside" as const, withoutEnlargement: true },
    quality: 92,
  },
] as const;

/** Format byte size for download labels (e.g. "4.2 MB"). */
export function formatFileSize(bytes: number): string {
  if (!(bytes >= 0) || !Number.isFinite(bytes)) return "0 B";
  if (bytes < 1000) return `${Math.round(bytes)} B`;
  if (bytes < 1000 * 1000) {
    const kb = bytes / 1000;
    const text = kb < 10 ? kb.toFixed(1) : String(Math.round(kb));
    return `${text} KB`;
  }
  const mb = bytes / (1000 * 1000);
  const text = mb < 10 ? mb.toFixed(1) : String(Math.round(mb));
  return `${text} MB`;
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

/** Copy only when dest is missing or size differs from source. */
function copyIfNeeded(from: string, to: string): void {
  fs.mkdirSync(path.dirname(to), { recursive: true });
  if (fs.existsSync(to)) {
    const fromSt = fs.statSync(from);
    const toSt = fs.statSync(to);
    if (fromSt.size === toSt.size) return;
  }
  fs.copyFileSync(from, to);
}

export async function processPhotoImages(
  srcPath: string,
  /** Site URL prefix without trailing slash, e.g. `/trips/2022-Cornwall` or `/photos`. */
  urlBase: string,
  filename: string,
  cacheDir: string,
  siteDir: string,
): Promise<ImageDerivatives> {
  const { key: src } = sourceKey(srcPath);
  const base = path.basename(filename);
  const stem = path.basename(base, path.extname(base));
  const outName = `${stem}.jpg`;
  const baseUrl = urlBase.replace(/\/$/, "");

  const specs = DERIVATIVES.map((d) => {
    const cacheFile = path.join(cacheDir, d.sub, outName);
    const metaFile = path.join(cacheDir, d.sub, `${outName}.key`);
    const key = `${src}_${d.keySuffix}`;
    const needsBuild =
      !(
        fs.existsSync(cacheFile) &&
        fs.existsSync(metaFile) &&
        fs.readFileSync(metaFile, "utf8") === key
      );
    const siteFile = path.join(siteDir, d.siteSub, outName);
    return { ...d, cacheFile, metaFile, key, needsBuild, siteFile };
  });

  const needsAny = specs.some((s) => s.needsBuild);
  const input = needsAny ? await fs.promises.readFile(srcPath) : null;

  for (const s of specs) {
    if (s.needsBuild) {
      if (!input) throw new Error(`Missing image buffer for ${srcPath}`);
      await writeIfNeeded(s.cacheFile, s.metaFile, s.key, async () => {
        await sharp(input, { failOn: "none" })
          .rotate()
          .resize(s.resize)
          .jpeg({ quality: s.quality, mozjpeg: true })
          .toFile(s.cacheFile);
      });
    }
    copyIfNeeded(s.cacheFile, s.siteFile);
  }

  const siteOriginal = path.join(siteDir, "originals", base);
  copyIfNeeded(srcPath, siteOriginal);

  return {
    gridThumbRel: `${baseUrl}/thumbs/grid/${outName}`,
    mapThumbRel: `${baseUrl}/thumbs/map/${outName}`,
    displayRel: `${baseUrl}/display/${outName}`,
    originalRel: `${baseUrl}/originals/${base}`,
  };
}
