import { formatDistanceKm } from "./gpx.ts";
import {
  defaultCreator,
  defaultTitle,
  enCreator,
  enTitle,
} from "../siteConstants.ts";
import type { SitePhoto, TripManifest, TripPhoto } from "./types.ts";

export type TripLang = "ru" | "en";

export type TripNameEntry = {
  lang: TripLang;
  name: string;
};

export type TripUi = {
  lang: TripLang;
  langPrefix: string;
  creator: string;
  siteTitle: string;
  listTitle: string;
  listEmpty: string;
  photoCount: (n: number) => string;
  trackCount: (n: number) => string;
  countsJoiner: string;
  allTrips: string;
  hideMap: string;
  showMap: string;
  hidePhotos: string;
  showPhotos: string;
  mapThumbsLabel: string;
  hideMapThumbs: string;
  showMapThumbs: string;
  distanceUnit: string;
  close: string;
  prevPhoto: string;
  nextPhoto: string;
  photoPage: string;
  relevantPosts: string;
  allPhotos: string;
  allPhotosBack: string;
};

export const tripUi: Record<TripLang, TripUi> = {
  ru: {
    lang: "ru",
    langPrefix: "",
    creator: defaultCreator,
    siteTitle: defaultTitle,
    listTitle: "Поездки",
    listEmpty: "Пока нет поездок.",
    photoCount: (n) => `${n} фото`,
    trackCount: (n) => (n > 1 ? `${n} трека` : `${n} трек`),
    countsJoiner: ", ",
    allTrips: "Все поездки",
    hideMap: "Скрыть карту",
    showMap: "Показать карту",
    hidePhotos: "Скрыть фото",
    showPhotos: "Показать фото",
    mapThumbsLabel: "Миниатюры на карте",
    hideMapThumbs: "Скрыть миниатюры на карте",
    showMapThumbs: "Показать миниатюры на карте",
    distanceUnit: "км",
    close: "Закрыть",
    prevPhoto: "Предыдущее фото",
    nextPhoto: "Следующее фото",
    photoPage: "Страница фото",
    relevantPosts: "Упоминания:",
    allPhotos: "Все фото",
    allPhotosBack: "Все фото",
  },
  en: {
    lang: "en",
    langPrefix: "en/",
    creator: enCreator,
    siteTitle: enTitle,
    listTitle: "Trips",
    listEmpty: "No trips yet.",
    photoCount: (n) => (n === 1 ? `${n} photo` : `${n} photos`),
    trackCount: (n) => (n === 1 ? `${n} track` : `${n} tracks`),
    countsJoiner: ", ",
    allTrips: "All trips",
    hideMap: "Hide map",
    showMap: "Show map",
    hidePhotos: "Hide photos",
    showPhotos: "Show photos",
    mapThumbsLabel: "Map thumbnails",
    hideMapThumbs: "Hide map thumbnails",
    showMapThumbs: "Show map thumbnails",
    distanceUnit: "km",
    close: "Close",
    prevPhoto: "Previous photo",
    nextPhoto: "Next photo",
    photoPage: "Photo page",
    relevantPosts: "Mentions:",
    allPhotos: "All photos",
    allPhotosBack: "All photos",
  },
};

export function isTripLang(value: unknown): value is TripLang {
  return value === "ru" || value === "en";
}

export function tripsBase(lang: TripLang): string {
  return `/${tripUi[lang].langPrefix}trips/`;
}

export function photosBase(lang: TripLang): string {
  return `/${tripUi[lang].langPrefix}photos/`;
}

export function resolveTripTitle(
  names: TripNameEntry[] | undefined,
  name: string | undefined,
  folderTitle: string,
  lang: TripLang,
): string {
  if (names && names.length > 0) {
    const match = names.find((entry) => entry.lang === lang);
    if (match) return match.name;
    return names[0]!.name;
  }
  if (name) return name;
  return folderTitle;
}

export function localizeSitePhoto(
  photo: SitePhoto,
  lang: TripLang,
): SitePhoto {
  if (photo.tripSlug) {
    const base = tripsBase(lang);
    return {
      ...photo,
      photoPageUrl: `${base}${photo.tripSlug}/photo/${photo.basename}.html`,
    };
  }
  return {
    ...photo,
    photoPageUrl: `${photosBase(lang)}${photo.basename}.html`,
  };
}

export function localizeAllPhotos(
  photos: SitePhoto[],
  lang: TripLang,
): SitePhoto[] {
  return photos.map((p) => localizeSitePhoto(p, lang));
}

function localizePhoto(
  photo: TripPhoto,
  base: string,
  slug: string,
): TripPhoto {
  return {
    ...photo,
    tripSlug: photo.tripSlug ?? slug,
    photoPageUrl: `${base}${slug}/photo/${photo.basename}.html`,
  };
}

/** Clone a manifest trip for a language: title, page URLs, distance label. */
export function localizeTrip(
  trip: TripManifest,
  lang: TripLang,
): TripManifest {
  const base = tripsBase(lang);
  const t = tripUi[lang];
  const photos = trip.photos.map((p) => localizePhoto(p, base, trip.slug));
  const mapPhotosJson = JSON.stringify(
    photos
      .filter(
        (p) =>
          typeof p.lat === "number" &&
          typeof p.lon === "number" &&
          Number.isFinite(p.lat) &&
          Number.isFinite(p.lon),
      )
      .map((p) => ({
        lat: p.lat,
        lon: p.lon,
        thumb: p.mapThumbUrl,
        display: p.displayUrl,
        url: p.photoPageUrl,
      })),
  );

  return {
    ...trip,
    title: resolveTripTitle(
      trip.names,
      trip.name,
      trip.folderTitle,
      lang,
    ),
    distanceKm: formatDistanceKm(trip.distanceMeters, t.distanceUnit),
    url: `${base}${trip.slug}/`,
    photos,
    mapPhotosJson,
  };
}

export function localizeTrips(
  trips: TripManifest[],
  lang: TripLang,
): TripManifest[] {
  return trips.map((trip) => localizeTrip(trip, lang));
}
