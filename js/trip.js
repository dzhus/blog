/* Trip map & interactive gallery logic */
(function () {
  // --- Leaflet Trip Map ---
  function initTripMap(el) {
    if (!window.L) return;
    var bounds = JSON.parse(el.getAttribute("data-bounds"));
    var tilesUrl = el.getAttribute("data-tiles");
    var zoom = Number(el.getAttribute("data-zoom"));
    var tracksUrl = el.getAttribute("data-tracks");
    var photos = JSON.parse(el.getAttribute("data-photos") || "[]");

    var southWest = L.latLng(bounds.south, bounds.west);
    var northEast = L.latLng(bounds.north, bounds.east);
    var latLngBounds = L.latLngBounds(southWest, northEast);

    var map = L.map(el, {
      crs: L.CRS.EPSG3857,
      minZoom: Math.max(0, zoom - 2),
      maxZoom: zoom + 1,
      maxBounds: latLngBounds,
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

    var tileLayer = L.tileLayer(tilesUrl, {
      minNativeZoom: zoom,
      maxNativeZoom: zoom,
      minZoom: Math.max(0, zoom - 2),
      maxZoom: zoom + 1,
      noWrap: true,
      bounds: latLngBounds,
      attribution: "",
    }).addTo(map);

    function fitTripMap() {
      map.invalidateSize({ animate: false });
      var coverZoom = map.getBoundsZoom(latLngBounds, true);
      map.setMinZoom(coverZoom);
      tileLayer.options.minZoom = coverZoom;
      map.setView(latLngBounds.getCenter(), coverZoom, { animate: false });
    }

    fitTripMap();
    el._tripMap = map;
    el._tripFit = fitTripMap;

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
      marker.on("click", function (e) {
        if (el._onMapPhotoClick) {
          if (e.originalEvent && typeof e.originalEvent.preventDefault === "function") {
            e.originalEvent.preventDefault();
          }
          el._onMapPhotoClick({ url: p.url, display: p.display });
        }
      });
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

    el.addEventListener("click", function (e) {
      var link = e.target.closest(".trip-map-photo-icon a");
      if (link) {
        e.preventDefault();
        var targetUrl = link.getAttribute("href");
        if (el._onMapPhotoClick) {
          el._onMapPhotoClick({ url: targetUrl });
        }
      }
    });
  }

  document.querySelectorAll(".trip-map[data-tiles]").forEach(initTripMap);

  // --- Trip layout chrome (optional) & photo gallery lightbox ---
  var layout = document.querySelector(".trip-layout");
  var galleryRoot =
    document.querySelector("[data-photo-gallery]") || layout;
  var grid =
    (layout && layout.querySelector(".trip-photo-grid")) ||
    (galleryRoot && galleryRoot.querySelector(".trip-photo-grid")) ||
    document.querySelector(".trip-photo-grid");
  var modal = document.getElementById("trip-photo-modal");
  if (!grid || !modal) return;

  var openPhotoModalByUrl = null;
  var catalogUrl =
    galleryRoot && galleryRoot.getAttribute("data-photos-catalog");

  if (layout) {
    var state = { map: true, photos: true };

    var hideMapBtn = layout.querySelector(".trip-chevron--map");
    var hidePhotosBtn = layout.querySelector(".trip-chevron--photos");
    var showMapBtn = layout.querySelector(".trip-chevron--show-map");
    var showPhotosBtn = layout.querySelector(".trip-chevron--show-photos");

    function apply() {
      layout.classList.toggle("trip-layout--map-hidden", !state.map);
      layout.classList.toggle("trip-layout--photos-hidden", !state.photos);

      if (hideMapBtn) hideMapBtn.hidden = !state.map || !state.photos;
      if (hidePhotosBtn) hidePhotosBtn.hidden = !state.photos || !state.map;
      if (showMapBtn) showMapBtn.hidden = state.map;
      if (showPhotosBtn) showPhotosBtn.hidden = state.photos;

      if (state.map) {
        requestAnimationFrame(function () {
          document.querySelectorAll(".trip-map").forEach(function (el) {
            if (el._tripFit) el._tripFit();
            else if (el._tripMap) el._tripMap.invalidateSize();
          });
        });
      }
    }

    function bind(btn, section, show) {
      if (!btn) return;
      btn.addEventListener("click", function () {
        if (show) {
          state[section] = true;
        } else {
          if (section === "map" && !state.photos) return;
          if (section === "photos" && !state.map) return;
          state[section] = false;
        }
        apply();
      });
    }

    bind(hideMapBtn, "map", false);
    bind(hidePhotosBtn, "photos", false);
    bind(showMapBtn, "map", true);
    bind(showPhotosBtn, "photos", true);

    var thumbsBtn = layout.querySelector(".trip-map-thumbs-toggle");
    if (thumbsBtn) {
      thumbsBtn.addEventListener("click", function () {
        var on = thumbsBtn.getAttribute("aria-pressed") !== "true";
        thumbsBtn.setAttribute("aria-pressed", on ? "true" : "false");
        var titleOn = thumbsBtn.getAttribute("data-title-on") || "";
        var titleOff = thumbsBtn.getAttribute("data-title-off") || "";
        thumbsBtn.title = on ? titleOn : titleOff;
        thumbsBtn.textContent = on ? "▣" : "□";
        document.querySelectorAll(".trip-map").forEach(function (el) {
          if (el._setTripPhotosVisible) el._setTripPhotosVisible(on);
        });
      });
    }

    var stage = layout.querySelector(".trip-photos-stage");
    var preview = layout.querySelector(".trip-photo-hover-preview");
    var previewImg = preview ? preview.querySelector("img") : null;

    grid.querySelectorAll(".trip-photo-grid-item[data-photo-url]").forEach(function (item) {
      item.addEventListener("mouseenter", function () {
        var url = item.getAttribute("data-photo-url");
        document.querySelectorAll(".trip-map").forEach(function (el) {
          if (el._focusTripPhoto) el._focusTripPhoto(url);
        });
      });
      item.addEventListener("mouseleave", function () {
        document.querySelectorAll(".trip-map").forEach(function (el) {
          if (el._focusTripPhoto) el._focusTripPhoto(null);
        });
      });
    });

    function setGridPhotoPreview(photo) {
      if (!stage || !preview || !previewImg) return;
      var url = photo && photo.url;
      var display = photo && photo.display;
      if (url && !display && grid) {
        var match = grid.querySelector(
          '.trip-photo-grid-item[data-photo-url="' + url.replace(/"/g, '\\"') + '"]'
        );
        if (match) {
          display = match.getAttribute("data-display-url");
        }
      }
      if (!url || !display) {
        stage.classList.remove("is-previewing");
        preview.removeAttribute("href");
        preview.removeAttribute("title");
        previewImg.removeAttribute("src");
        previewImg.removeAttribute("title");
        previewImg.alt = "";
        return;
      }
      previewImg.src = display;
      previewImg.alt = "";
      previewImg.removeAttribute("title");
      preview.href = url;
      preview.removeAttribute("title");
      stage.classList.add("is-previewing");
    }

    document.querySelectorAll(".trip-map").forEach(function (el) {
      el._onMapPhotoHover = setGridPhotoPreview;
      el._onMapPhotoClick = function (photo) {
        var url = photo && photo.url;
        if (url && typeof openPhotoModalByUrl === "function") {
          openPhotoModalByUrl(url);
        }
      };
    });

    if (preview) {
      preview.addEventListener("click", function (e) {
        e.preventDefault();
        var targetUrl = preview.getAttribute("href");
        if (targetUrl) {
          openPhotoModalByUrl(targetUrl);
        }
      });
    }
  }

  // Photo modal & lightbox
  var gridItems = Array.from(grid.querySelectorAll(".trip-photo-grid-item"));

  function photoFromGridItem(item, fallbackIndex) {
    var idxAttr = item.getAttribute("data-photo-index");
    var index =
      idxAttr != null && idxAttr !== "" ? Number(idxAttr) : fallbackIndex;
    return {
      index: index,
      id: item.getAttribute("data-photo-index") || String(index),
      displayUrl: item.getAttribute("data-display-url") || "",
      originalUrl: item.getAttribute("data-original-url") || "",
      filename: item.getAttribute("data-filename") || "",
      originalSize: item.getAttribute("data-original-size") || "",
      tooltip: item.getAttribute("title") || "",
      exifLine: item.getAttribute("data-exif-line") || "",
      photoUrl: item.getAttribute("data-photo-url") || ""
    };
  }

  var photos = gridItems.map(function (item, idx) {
    return photoFromGridItem(item, idx);
  });

  var totalPhotos = photos.length;
  var currentPhotoIndex = 0;
  var catalogReady = !catalogUrl;

  var modalCloseBtn = modal.querySelector(".trip-modal-close");
  var modalSingleImg = modal.querySelector(".trip-modal-single-img");
  var modalSingleExif = modal.querySelector(".trip-modal-single-exif");
  var modalSingleDownload = modal.querySelector(".trip-modal-single-download");
  var modalSinglePos = modal.querySelector(".trip-modal-single-pos");
  var modalPrevBtn = modal.querySelector(".trip-modal-prev");
  var modalNextBtn = modal.querySelector(".trip-modal-next");
  var modalPageLink = modal.querySelector("#trip-modal-page-link");
  var modalFeed = modal.querySelector(".trip-modal-feed");
  var feedItemByIndex = Object.create(null);
  var pageLinkLabel = modalPageLink ? modalPageLink.textContent : "🔗";

  if (modalFeed) {
    Array.from(modalFeed.querySelectorAll(".trip-modal-feed-item")).forEach(
      function (item) {
        var idx = Number(item.getAttribute("data-photo-index"));
        if (!Number.isNaN(idx)) feedItemByIndex[idx] = item;
      }
    );
  }

  function isMobile() {
    return window.matchMedia("(max-width: 52rem)").matches;
  }

  function applyPhotosList(list) {
    photos = list;
    totalPhotos = photos.length;
  }

  function loadCatalog() {
    if (!catalogUrl || catalogReady) {
      return Promise.resolve();
    }
    return fetch(catalogUrl)
      .then(function (r) {
        return r.json();
      })
      .then(function (data) {
        var rows = (data && data.photos) || [];
        applyPhotosList(
          rows.map(function (row, idx) {
            return {
              index: idx,
              id: row.id || String(idx),
              displayUrl: row.displayUrl || "",
              originalUrl: row.originalUrl || "",
              filename: row.filename || "",
              originalSize: row.originalSize || "",
              tooltip: row.exifLine || "",
              exifLine: row.exifLine || "",
              photoUrl: row.photoPageUrl || ""
            };
          })
        );
        catalogReady = true;
        if (galleryRoot) {
          galleryRoot.classList.add("photo-gallery--catalog");
        }
      })
      .catch(function () {
        catalogReady = true;
      });
  }

  function renderDesktopPhoto(index) {
    if (index < 0 || index >= totalPhotos) return;
    currentPhotoIndex = index;
    var p = photos[index];

    modalSingleImg.src = p.displayUrl;
    modalSingleImg.alt = "";
    modalSingleImg.removeAttribute("title");

    if (modalSingleExif) {
      modalSingleExif.textContent = p.exifLine || "";
    }

    if (modalPageLink) {
      modalPageLink.href = p.photoUrl;
    }

    if (modalSingleDownload) {
      modalSingleDownload.href = p.originalUrl;
      modalSingleDownload.download = p.filename;
      modalSingleDownload.textContent =
        p.filename + " (" + p.originalSize + ")";
    }

    if (modalSinglePos) {
      modalSinglePos.textContent = " · " + (index + 1) + "/" + totalPhotos;
    }

    if (modalPrevBtn) {
      modalPrevBtn.hidden = index <= 0;
    }
    if (modalNextBtn) {
      modalNextBtn.hidden = index >= totalPhotos - 1;
    }

    if (index + 1 < totalPhotos) {
      var preNext = new Image();
      preNext.src = photos[index + 1].displayUrl;
    }
    if (index - 1 >= 0) {
      var prePrev = new Image();
      prePrev.src = photos[index - 1].displayUrl;
    }
  }

  openPhotoModalByUrl = function (url) {
    if (!url) return;
    loadCatalog().then(function () {
      var foundIdx = photos.findIndex(function (p) {
        return p.photoUrl === url;
      });
      if (foundIdx !== -1) {
        openModal(foundIdx);
      }
    });
  };

  function loadFeedImage(item) {
    if (!item) return;
    var img = item.querySelector(".trip-modal-feed-img");
    if (img) {
      var dataSrc = img.getAttribute("data-src");
      if (dataSrc) {
        img.src = dataSrc;
        img.removeAttribute("data-src");
      }
    }
  }

  var feedObserver = null;
  function initFeedObserver() {
    if (feedObserver) return;
    if (!modalFeed) return;
    if ("IntersectionObserver" in window) {
      feedObserver = new IntersectionObserver(
        function (entries, observer) {
          entries.forEach(function (entry) {
            if (entry.isIntersecting) {
              loadFeedImage(entry.target);
              observer.unobserve(entry.target);
            }
          });
        },
        {
          root: modalFeed,
          rootMargin: "300px 0px"
        }
      );

      Object.keys(feedItemByIndex).forEach(function (key) {
        feedObserver.observe(feedItemByIndex[key]);
      });
    } else {
      Object.keys(feedItemByIndex).forEach(function (key) {
        loadFeedImage(feedItemByIndex[key]);
      });
    }
  }

  function ensureFeedItem(index) {
    if (!modalFeed || index < 0 || index >= totalPhotos) return null;
    if (feedItemByIndex[index]) return feedItemByIndex[index];
    var p = photos[index];
    if (!p) return null;

    var fig = document.createElement("figure");
    fig.className = "trip-modal-feed-item";
    fig.setAttribute("data-photo-id", p.id);
    fig.setAttribute("data-photo-index", String(index));
    fig.setAttribute("data-photo-url", p.photoUrl);

    var img = document.createElement("img");
    img.className = "trip-modal-feed-img";
    img.setAttribute("data-src", p.displayUrl);
    img.alt = "";
    img.loading = "lazy";
    fig.appendChild(img);

    var cap = document.createElement("figcaption");
    cap.className = "trip-modal-feed-caption";
    if (p.exifLine) {
      var exif = document.createElement("div");
      exif.className = "trip-modal-feed-exif";
      exif.textContent = p.exifLine;
      cap.appendChild(exif);
    }
    var meta = document.createElement("div");
    meta.className = "trip-modal-feed-meta";
    var pageA = document.createElement("a");
    pageA.href = p.photoUrl;
    pageA.textContent = pageLinkLabel;
    meta.appendChild(pageA);
    meta.appendChild(document.createTextNode(" · "));
    var dl = document.createElement("a");
    dl.href = p.originalUrl;
    dl.download = p.filename;
    dl.textContent = p.filename + " (" + p.originalSize + ")";
    meta.appendChild(dl);
    cap.appendChild(meta);
    fig.appendChild(cap);

    var inserted = false;
    var children = Array.from(modalFeed.children);
    for (var c = 0; c < children.length; c++) {
      var siblingIdx = Number(children[c].getAttribute("data-photo-index"));
      if (!Number.isNaN(siblingIdx) && siblingIdx > index) {
        modalFeed.insertBefore(fig, children[c]);
        inserted = true;
        break;
      }
    }
    if (!inserted) modalFeed.appendChild(fig);

    feedItemByIndex[index] = fig;
    if (feedObserver) feedObserver.observe(fig);
    return fig;
  }

  function ensureFeedRange(center) {
    var from = Math.max(0, center - 2);
    var to = Math.min(totalPhotos - 1, center + 2);
    for (var i = from; i <= to; i++) {
      ensureFeedItem(i);
    }
  }

  function updateMobileActivePhoto() {
    if (!modalFeed) return;
    var feedRect = modalFeed.getBoundingClientRect();
    var midY = feedRect.top + feedRect.height / 2;
    var closestIdx = currentPhotoIndex;
    var closestDist = Infinity;
    var keys = Object.keys(feedItemByIndex);
    for (var k = 0; k < keys.length; k++) {
      var i = Number(keys[k]);
      var el = feedItemByIndex[i];
      if (!el) continue;
      var rect = el.getBoundingClientRect();
      var itemMidY = rect.top + rect.height / 2;
      var dist = Math.abs(itemMidY - midY);
      if (dist < closestDist) {
        closestDist = dist;
        closestIdx = i;
      }
    }
    if (photos[closestIdx]) {
      currentPhotoIndex = closestIdx;
      if (modalPageLink) {
        modalPageLink.href = photos[closestIdx].photoUrl;
      }
      ensureFeedRange(closestIdx);
    }
  }

  var scrollTicking = false;
  function onModalFeedScroll() {
    if (!scrollTicking) {
      window.requestAnimationFrame(function () {
        updateMobileActivePhoto();
        scrollTicking = false;
      });
      scrollTicking = true;
    }
  }
  if (modalFeed) {
    modalFeed.addEventListener("scroll", onModalFeedScroll, { passive: true });
  }

  function openModal(index) {
    currentPhotoIndex = index;
    document.body.style.overflow = "hidden";

    if (modalPageLink && photos[index]) {
      modalPageLink.href = photos[index].photoUrl;
    }

    if (typeof modal.showModal === "function") {
      if (!modal.open) modal.showModal();
    } else {
      modal.setAttribute("open", "");
    }

    if (isMobile()) {
      initFeedObserver();
      ensureFeedRange(index);
      var item = feedItemByIndex[index];
      if (item) {
        loadFeedImage(item);
        if (feedItemByIndex[index - 1]) loadFeedImage(feedItemByIndex[index - 1]);
        if (feedItemByIndex[index + 1]) loadFeedImage(feedItemByIndex[index + 1]);
        item.scrollIntoView({ block: "start" });
        setTimeout(function () {
          if (feedItemByIndex[index]) {
            feedItemByIndex[index].scrollIntoView({ block: "start" });
          }
        }, 30);
      }
    } else {
      renderDesktopPhoto(index);
    }
  }

  function closeModal() {
    document.body.style.overflow = "";
    if (typeof modal.close === "function") {
      if (modal.open) modal.close();
    } else {
      modal.removeAttribute("open");
    }
  }

  gridItems.forEach(function (item) {
    item.addEventListener("click", function (e) {
      e.preventDefault();
      var idxAttr = item.getAttribute("data-photo-index");
      var idx =
        idxAttr != null && idxAttr !== ""
          ? Number(idxAttr)
          : gridItems.indexOf(item);
      loadCatalog().then(function () {
        openModal(idx);
      });
    });
  });

  if (modalCloseBtn) {
    modalCloseBtn.addEventListener("click", function (e) {
      e.stopPropagation();
      closeModal();
    });
  }

  if (modalPrevBtn) {
    modalPrevBtn.addEventListener("click", function (e) {
      e.stopPropagation();
      if (currentPhotoIndex > 0) {
        renderDesktopPhoto(currentPhotoIndex - 1);
      }
    });
  }

  if (modalNextBtn) {
    modalNextBtn.addEventListener("click", function (e) {
      e.stopPropagation();
      if (currentPhotoIndex < totalPhotos - 1) {
        renderDesktopPhoto(currentPhotoIndex + 1);
      }
    });
  }

  document.addEventListener("keydown", function (e) {
    if (!modal.open && !modal.hasAttribute("open")) return;
    if (e.key === "Escape" || e.keyCode === 27) {
      closeModal();
    } else if (!isMobile()) {
      if (e.key === "ArrowLeft" || e.keyCode === 37) {
        if (currentPhotoIndex > 0) {
          renderDesktopPhoto(currentPhotoIndex - 1);
        }
      } else if (e.key === "ArrowRight" || e.keyCode === 39) {
        if (currentPhotoIndex < totalPhotos - 1) {
          renderDesktopPhoto(currentPhotoIndex + 1);
        }
      }
    }
  });

  modal.addEventListener("cancel", function () {
    document.body.style.overflow = "";
  });

  modal.addEventListener("close", function () {
    document.body.style.overflow = "";
  });

  modal.addEventListener("click", function (e) {
    if (!isMobile()) {
      if (
        e.target === modal ||
        e.target.classList.contains("trip-modal-single") ||
        e.target.classList.contains("trip-modal-single-figure")
      ) {
        closeModal();
      }
    }
  });

  if (modalPageLink) {
    modalPageLink.addEventListener("click", function (e) {
      e.stopPropagation();
    });
  }

  window.addEventListener("resize", function () {
    if (modal.open || modal.hasAttribute("open")) {
      if (isMobile()) {
        initFeedObserver();
        ensureFeedRange(currentPhotoIndex);
        updateMobileActivePhoto();
      } else {
        renderDesktopPhoto(currentPhotoIndex);
      }
    }
  });

  if (catalogUrl) {
    loadCatalog();
  }
})();
