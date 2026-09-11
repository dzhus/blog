import fs from "node:fs";
import path from "node:path";

export function vendorLeaflet(
  projectRoot: string,
  siteRoot: string,
): void {
  const srcDir = path.join(projectRoot, "node_modules", "leaflet", "dist");
  const destDir = path.join(siteRoot, "vendor", "leaflet");

  if (!fs.existsSync(srcDir)) {
    throw new Error("leaflet package not found; run npm install");
  }

  fs.mkdirSync(destDir, { recursive: true });

  for (const name of ["leaflet.js", "leaflet.css"]) {
    fs.copyFileSync(path.join(srcDir, name), path.join(destDir, name));
  }

  const imgSrc = path.join(srcDir, "images");
  const imgDest = path.join(destDir, "images");
  fs.mkdirSync(imgDest, { recursive: true });
  if (fs.existsSync(imgSrc)) {
    for (const f of fs.readdirSync(imgSrc)) {
      fs.copyFileSync(path.join(imgSrc, f), path.join(imgDest, f));
    }
  }

  // Fix CSS url(images/...) — already relative to leaflet.css location; OK.
  const notice = `Leaflet ${readLeafletVersion(projectRoot)}
Copyright (c) Volodymyr Agafonkin, Leaflet contributors
https://leafletjs.com/
License: BSD-2-Clause (see leaflet package)

Map tiles used to generate static basemap snapshots:
© OpenStreetMap contributors
© OpenTopoMap (CC-BY-SA) — https://opentopomap.org/
https://www.openstreetmap.org/copyright
`;
  fs.writeFileSync(path.join(destDir, "ATTRIBUTION.txt"), notice);
}

function readLeafletVersion(projectRoot: string): string {
  try {
    const pkg = JSON.parse(
      fs.readFileSync(
        path.join(projectRoot, "node_modules", "leaflet", "package.json"),
        "utf8",
      ),
    );
    return String(pkg.version ?? "");
  } catch {
    return "";
  }
}
