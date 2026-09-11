import fs from "node:fs";
import path from "node:path";

export function writeMapScript(outPath: string): void {
  const script = `/* Trip map widget — local Leaflet only, no remote tiles */
(function () {
  function init(el) {
    var bounds = JSON.parse(el.getAttribute("data-bounds"));
    var basemapUrl = el.getAttribute("data-basemap");
    var tracksUrl = el.getAttribute("data-tracks");
    var photos = JSON.parse(el.getAttribute("data-photos") || "[]");

    var southWest = L.latLng(bounds.south, bounds.west);
    var northEast = L.latLng(bounds.north, bounds.east);
    var latLngBounds = L.latLngBounds(southWest, northEast);

    var map = L.map(el, {
      crs: L.CRS.EPSG3857,
      maxBounds: latLngBounds.pad(0.05),
      maxBoundsViscosity: 1.0,
      scrollWheelZoom: true,
      attributionControl: true,
    });

    map.attributionControl.setPrefix(
      '<a href="/vendor/leaflet/ATTRIBUTION.txt">Leaflet</a>'
    );
    map.attributionControl.addAttribution(
      '© <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>, <a href="https://opentopomap.org">OpenTopoMap</a> (CC-BY-SA)'
    );

    L.imageOverlay(basemapUrl, latLngBounds, { opacity: 1, interactive: false }).addTo(map);
    map.fitBounds(latLngBounds);

    fetch(tracksUrl)
      .then(function (r) { return r.json(); })
      .then(function (fc) {
        (fc.features || []).forEach(function (f) {
          var coords = f.leafletCoordinates;
          if (!coords || !coords.length) {
            coords = (f.geometry.coordinates || []).map(function (c) {
              return [c[1], c[0]];
            });
          }
          L.polyline(coords, {
            color: f.properties.color,
            weight: 3,
            opacity: 0.9,
            lineJoin: "round",
          }).addTo(map);
        });
      })
      .catch(function () { /* tracks optional at runtime */ });

    photos.forEach(function (p) {
      if (p.lat == null || p.lon == null) return;
      var icon = L.divIcon({
        className: "trip-map-photo-icon",
        html: '<a href="' + p.url + '"><img src="' + p.thumb + '" alt="" width="40" height="40"/></a>',
        iconSize: [44, 44],
        iconAnchor: [22, 22],
      });
      L.marker([p.lat, p.lon], { icon: icon }).addTo(map);
    });
  }

  document.querySelectorAll(".trip-map[data-bounds]").forEach(init);
})();
`;
  fs.mkdirSync(path.dirname(outPath), { recursive: true });
  fs.writeFileSync(outPath, script);
}
