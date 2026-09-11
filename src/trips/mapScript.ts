import fs from "node:fs";
import path from "node:path";

export function writeMapScript(outPath: string): void {
  const script = `/* Trip map — local greyscale XYZ tiles + EPSG:3857 (no remote tiles) */
(function () {
  function init(el) {
    var bounds = JSON.parse(el.getAttribute("data-bounds"));
    var tileBoundsRaw = el.getAttribute("data-tile-bounds");
    var tileBounds = tileBoundsRaw ? JSON.parse(tileBoundsRaw) : bounds;
    var tilesUrl = el.getAttribute("data-tiles");
    var zoom = Number(el.getAttribute("data-zoom"));
    var tracksUrl = el.getAttribute("data-tracks");
    var photos = JSON.parse(el.getAttribute("data-photos") || "[]");

    var fitLatLngBounds = L.latLngBounds(
      L.latLng(bounds.south, bounds.west),
      L.latLng(bounds.north, bounds.east)
    );
    var tileLatLngBounds = L.latLngBounds(
      L.latLng(tileBounds.south, tileBounds.west),
      L.latLng(tileBounds.north, tileBounds.east)
    );

    // Integer zoomSnap: smooth wheel zoom and no fractional-scale tile seams.
    // Tiles are fetched oversized (see expandBBoxForIntegerContain) so contain
    // fit at floor(z) still has coverage across the whole pane.
    var map = L.map(el, {
      crs: L.CRS.EPSG3857,
      zoomSnap: 1,
      zoomDelta: 1,
      minZoom: Math.max(0, zoom - 2),
      maxZoom: zoom + 1,
      scrollWheelZoom: true,
      attributionControl: true,
    });

    map.attributionControl.setPrefix(
      '<a href="/vendor/leaflet/ATTRIBUTION.txt">Leaflet</a>'
    );
    map.attributionControl.addAttribution(
      '© <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>, <a href="https://opentopomap.org">OpenTopoMap</a> (CC-BY-SA)'
    );

    L.tileLayer(tilesUrl, {
      minNativeZoom: zoom,
      maxNativeZoom: zoom,
      minZoom: Math.max(0, zoom - 2),
      maxZoom: zoom + 1,
      noWrap: true,
      bounds: tileLatLngBounds,
      attribution: "",
    }).addTo(map);

    function fitTripBounds() {
      map.invalidateSize({ animate: false });
      map.fitBounds(fitLatLngBounds, { animate: false, padding: [0, 0] });
    }

    fitTripBounds();
    el._tripMap = map;
    el._tripFit = fitTripBounds;

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
      .catch(function () {});

    var photoLayer = L.layerGroup();
    var photoMarkers = [];
    var photosVisible = true;
    var focusedUrl = null;

    function syncPhotoMarkers() {
      photoMarkers.forEach(function (item) {
        var show = photosVisible && (focusedUrl == null || item.url === focusedUrl);
        if (show) {
          if (!photoLayer.hasLayer(item.marker)) photoLayer.addLayer(item.marker);
        } else if (photoLayer.hasLayer(item.marker)) {
          photoLayer.removeLayer(item.marker);
        }
      });
      if (photosVisible) {
        if (!map.hasLayer(photoLayer)) photoLayer.addTo(map);
      } else if (map.hasLayer(photoLayer)) {
        map.removeLayer(photoLayer);
      }
    }

    photos.forEach(function (p) {
      if (p.lat == null || p.lon == null) return;
      var icon = L.divIcon({
        className: "trip-map-photo-icon",
        html: '<a href="' + p.url + '"><img src="' + p.thumb + '" alt="" width="40" height="40"/></a>',
        iconSize: [44, 44],
        iconAnchor: [22, 22],
      });
      var marker = L.marker([p.lat, p.lon], { icon: icon });
      marker.on("mouseover", function () {
        if (el._onMapPhotoHover) {
          el._onMapPhotoHover({ url: p.url, display: p.display });
        }
      });
      marker.on("mouseout", function () {
        if (el._onMapPhotoHover) el._onMapPhotoHover(null);
      });
      photoMarkers.push({ url: p.url, marker: marker });
      photoLayer.addLayer(marker);
    });
    photoLayer.addTo(map);
    el._tripPhotoLayer = photoLayer;

    el._setTripPhotosVisible = function (visible) {
      photosVisible = !!visible;
      if (!photosVisible) focusedUrl = null;
      syncPhotoMarkers();
    };

    el._focusTripPhoto = function (url) {
      if (!photosVisible) return;
      focusedUrl = url || null;
      syncPhotoMarkers();
    };
  }

  document.querySelectorAll(".trip-map[data-tiles]").forEach(init);
})();
`;
  fs.mkdirSync(path.dirname(outPath), { recursive: true });
  fs.writeFileSync(outPath, script);
}
