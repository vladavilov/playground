export const SOURCE_LANGUAGES = ["javascript", "java", "cobol"] as const;

export type SourceLanguage = (typeof SOURCE_LANGUAGES)[number];

export function isSourceLanguage(v: string): v is SourceLanguage {
  return (SOURCE_LANGUAGES as readonly string[]).includes(v);
}

export function extractGitlabPath(url: string | null) {
  if (!url) return null;
  try {
    const parsed = new URL(url);
    return parsed.pathname.replace(/^\//, "").replace(/\.git$/, "");
  } catch {
    return url.replace(/\.git$/, "");
  }
}

export function isOkResponse(v: unknown): v is { ok: true } {
  return Boolean(v && typeof v === "object" && (v as { ok?: unknown }).ok === true);
}


