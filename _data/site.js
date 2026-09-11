import { createHash } from "node:crypto";

const email = "dima@dzhus.org";

export default {
  rootUrl: "https://dzhus.org",
  email,
  gravatar:
    "https://www.gravatar.com/avatar/" +
    createHash("md5").update(email).digest("hex") +
    "?s=200",
  thisYear: String(new Date().getFullYear()),
  feedAuthorName: "Dmitry Dzhus",
  feedTitle: "Dmitry's journal",
};
