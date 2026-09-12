import { createHash } from "node:crypto";

export const email = "dima@dzhus.org";
export const rootUrl = "https://dzhus.org";
export const defaultLang = "ru";

export const defaultCreator = "Дмитрий Джус";
export const defaultTitle = "Журнал Дмитрия";
export const enCreator = "Dmitry Dzhus";
export const enTitle = "Dmitry's journal";

export const gravatar =
  "https://www.gravatar.com/avatar/" +
  createHash("md5").update(email).digest("hex") +
  "?s=200";

export const feedAuthorName = "Dmitry Dzhus";
export const feedTitle = "Dmitry's journal";

export type SiteLang = "ru" | "en";

export type LanguageDefinition = {
  lang: SiteLang;
  langPrefix: string;
  creator: string;
  siteTitle: string;
  latestPosts: string;
  allPosts: string;
  allPostsUrl: string;
  trips: string;
  tripsUrl: string;
  switchLangLabel: string;
  switchLangUrl: string;
  tags: string;
  tagsUrl: string;
};

export const languages: Record<SiteLang, LanguageDefinition> = {
  ru: {
    lang: "ru",
    langPrefix: "",
    creator: defaultCreator,
    siteTitle: defaultTitle,
    latestPosts: "Последние записи:",
    allPosts: "Все записи одним списком",
    allPostsUrl: "/posts/index.html",
    trips: "Поездки",
    tripsUrl: "/trips/index.html",
    switchLangLabel: "English",
    switchLangUrl: "/en/index.html",
    tags: "Темы",
    tagsUrl: "/tag/index.html",
  },
  en: {
    lang: "en",
    langPrefix: "en/",
    creator: enCreator,
    siteTitle: enTitle,
    latestPosts: "Latest posts:",
    allPosts: "Everything as one list",
    allPostsUrl: "/en/posts/index.html",
    trips: "Trips",
    tripsUrl: "/en/trips/index.html",
    switchLangLabel: "По-русски",
    switchLangUrl: "/index.html",
    tags: "Tags",
    tagsUrl: "/tag/index.html",
  },
};

export const languageDefinitions = languages;

export function localizedMeta(lang: string | undefined): LanguageDefinition {
  return lang === "en" ? languages.en : languages.ru;
}

export function thisYear(): string {
  return String(new Date().getFullYear());
}
