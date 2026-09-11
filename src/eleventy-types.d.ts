declare module "@11ty/eleventy" {
  export interface UserConfig {
    addPlugin(plugin: unknown, options?: unknown): void;
    setQuietMode(quiet: boolean): void;
    ignores: { add(pattern: string): void };
    addPassthroughCopy(path: string | Record<string, string>): void;
    addGlobalData(name: string, data: unknown): void;
    addCollection(
      name: string,
      callback: (api: CollectionApi) => unknown,
    ): void;
    addFilter(name: string, callback: (...args: any[]) => unknown): void;
    addPreprocessor(
      name: string,
      formats: string,
      callback: (data: any, content: string) => string | undefined,
    ): void;
    setLibrary(name: string, library: unknown): void;
    on(event: string, callback: () => void | Promise<void>): void;
  }

  export interface CollectionApi {
    getFilteredByGlob(glob: string): CollectionItem[];
  }

  export interface CollectionItem {
    url: string;
    date: Date;
    data: Record<string, any>;
    templateContent?: string;
  }
}

declare module "@11ty/eleventy-plugin-rss" {
  const plugin: unknown;
  export default plugin;
}
