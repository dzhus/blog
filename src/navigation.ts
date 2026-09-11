export type NavFields = {
  previousPageUrl?: string;
  nextPageUrl?: string;
  currentPageNum: number;
  numPages: number;
};

type CollectionItem = {
  url: string;
  date?: Date;
  data: { date?: Date; page?: { date?: Date } };
};

/** Chronological (oldest first) neighbour links within one language. */
export function computeNavigation(
  currentUrl: string,
  collectionChronological: CollectionItem[],
): NavFields {
  const items = [...collectionChronological].sort((a, b) => {
    const da = +getDate(a);
    const db = +getDate(b);
    return da - db;
  });
  const numPages = items.length;
  const index = items.findIndex((item) => item.url === currentUrl);
  if (index < 0) {
    return { currentPageNum: 0, numPages };
  }
  const result: NavFields = {
    currentPageNum: index + 1,
    numPages,
  };
  if (index > 0) {
    result.previousPageUrl = items[index - 1].url;
  }
  if (index < items.length - 1) {
    result.nextPageUrl = items[index + 1].url;
  }
  return result;
}

function getDate(item: CollectionItem): Date {
  return item.data.date ?? item.date ?? item.data.page?.date ?? new Date(0);
}
