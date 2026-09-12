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
      maxBounds: latLngBounds.pad(0.08),
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

    L.tileLayer(tilesUrl, {
      minNativeZoom: zoom,
      maxNativeZoom: zoom,
      minZoom: Math.max(0, zoom - 2),
      maxZoom: zoom + 1,
      noWrap: true,
      bounds: latLngBounds,
      attribution: "",
    }).addTo(map);

    map.fitBounds(latLngBounds);
    el._tripMap = map;
    el._tripFit = function () {
      map.invalidateSize();
      map.fitBounds(latLngBounds);
    };

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

  // --- Trip Layout & Gallery Lightbox ---
  var layout = document.querySelector(".trip-layout");
  if (!layout) return;

  var openPhotoModalByUrl = null;

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
  var grid = layout.querySelector(".trip-photo-grid");
  var preview = layout.querySelector(".trip-photo-hover-preview");
  var previewImg = preview ? preview.querySelector("img") : null;

  if (grid) {
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
  }

  function setGridPhotoPreview(photo) {
    if (!stage || !preview || !previewImg) return;
    var previewExif = preview.querySelector(".trip-photo-hover-preview-exif");
    var url = photo && photo.url;
    var display = photo && photo.display;
    var exifLine = "";
    if (url && grid) {
      var match = grid.querySelector(
        '.trip-photo-grid-item[data-photo-url="' + url.replace(/"/g, '\\"') + '"]'
      );
      if (match) {
        if (!display) display = match.getAttribute("data-display-url");
        exifLine = match.getAttribute("data-exif-line") || "";
      }
    }
    if (!url || !display) {
      stage.classList.remove("is-previewing");
      preview.removeAttribute("href");
      preview.removeAttribute("title");
      previewImg.removeAttribute("src");
      previewImg.removeAttribute("title");
      previewImg.alt = "";
      if (previewExif) previewExif.textContent = "";
      return;
    }
    previewImg.src = display;
    previewImg.alt = "";
    previewImg.removeAttribute("title");
    preview.href = url;
    preview.removeAttribute("title");
    if (previewExif) previewExif.textContent = exifLine;
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

  // Photo modal & lightbox
  var modal = document.getElementById("trip-photo-modal");
  if (modal && grid) {
    var gridItems = Array.from(grid.querySelectorAll(".trip-photo-grid-item"));
    var photos = gridItems.map(function (item, idx) {
      return {
        index: idx,
        id: item.getAttribute("data-photo-index") || String(idx),
        displayUrl: item.getAttribute("data-display-url") || "",
        originalUrl: item.getAttribute("data-original-url") || "",
        filename: item.getAttribute("data-filename") || "",
        originalSize: item.getAttribute("data-original-size") || "",
        tooltip: item.getAttribute("title") || "",
        exifLine: item.getAttribute("data-exif-line") || "",
        photoUrl: item.getAttribute("data-photo-url") || ""
      };
    });

    var totalPhotos = photos.length;
    var currentPhotoIndex = 0;

    var modalCloseBtn = modal.querySelector(".trip-modal-close");
    var modalSingleImg = modal.querySelector(".trip-modal-single-img");
    var modalSingleExif = modal.querySelector(".trip-modal-single-exif");
    var modalSingleDownload = modal.querySelector(".trip-modal-single-download");
    var modalSinglePos = modal.querySelector(".trip-modal-single-pos");
    var modalPrevBtn = modal.querySelector(".trip-modal-prev");
    var modalNextBtn = modal.querySelector(".trip-modal-next");
    var modalPageLink = modal.querySelector("#trip-modal-page-link");
    var modalFeed = modal.querySelector(".trip-modal-feed");
    var feedItems = modalFeed ? Array.from(modalFeed.querySelectorAll(".trip-modal-feed-item")) : [];

    function isMobile() {
      return window.matchMedia("(max-width: 52rem)").matches;
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
        modalSingleDownload.textContent = p.filename + " (" + p.originalSize + ")";
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

      // Preload adjacent images
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
      if (!url || !photos) return;
      var foundIdx = photos.findIndex(function (p) {
        return p.photoUrl === url;
      });
      if (foundIdx !== -1) {
        openModal(foundIdx);
      }
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

    // Lazy load observer for mobile feed
    var feedObserver = null;
    function initFeedObserver() {
      if (feedObserver) return;
      if ("IntersectionObserver" in window) {
        feedObserver = new IntersectionObserver(function (entries, observer) {
          entries.forEach(function (entry) {
            if (entry.isIntersecting) {
              loadFeedImage(entry.target);
              observer.unobserve(entry.target);
            }
          });
        }, {
          root: modalFeed,
          rootMargin: "300px 0px"
        });

        feedItems.forEach(function (item) {
          feedObserver.observe(item);
        });
      } else {
        feedItems.forEach(loadFeedImage);
      }
    }

    function updateMobileActivePhoto() {
      if (!modalFeed || !feedItems.length) return;
      var feedRect = modalFeed.getBoundingClientRect();
      var midY = feedRect.top + feedRect.height / 2;
      var closestIdx = currentPhotoIndex;
      var closestDist = Infinity;
      for (var i = 0; i < feedItems.length; i++) {
        var rect = feedItems[i].getBoundingClientRect();
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
        if (feedItems[index]) {
          loadFeedImage(feedItems[index]);
          if (feedItems[index - 1]) loadFeedImage(feedItems[index - 1]);
          if (feedItems[index + 1]) loadFeedImage(feedItems[index + 1]);
          feedItems[index].scrollIntoView({ block: "start" });
          setTimeout(function () {
            feedItems[index].scrollIntoView({ block: "start" });
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

    // Grid clicks
    gridItems.forEach(function (item, idx) {
      item.addEventListener("click", function (e) {
        e.preventDefault();
        openModal(idx);
      });
    });

    // Hover preview click (if preview is active)
    if (preview) {
      preview.addEventListener("click", function (e) {
        e.preventDefault();
        var targetUrl = preview.getAttribute("href");
        if (targetUrl) {
          openPhotoModalByUrl(targetUrl);
        }
      });
    }

    // Close button
    if (modalCloseBtn) {
      modalCloseBtn.addEventListener("click", function (e) {
        e.stopPropagation();
        closeModal();
      });
    }

    // Navigation buttons
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

    // Keyboard navigation
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

    // Backdrop click on desktop
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
          updateMobileActivePhoto();
        } else {
          renderDesktopPhoto(currentPhotoIndex);
        }
      }
    });
  }
})();
