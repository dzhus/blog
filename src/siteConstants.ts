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

export function localizedMeta(lang: string | undefined) {
  const isEn = lang === "en";
  return {
    lang: isEn ? "en" : defaultLang,
    langPrefix: isEn ? "en/" : "",
    creator: isEn ? enCreator : defaultCreator,
    siteTitle: isEn ? enTitle : defaultTitle,
  };
}

export function thisYear(): string {
  return String(new Date().getFullYear());
}
