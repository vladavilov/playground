import { afterEach, describe, expect, it } from "vitest";

import { useUiStore } from "./store";

afterEach(() => {
  useUiStore.getState().reset();
});

describe("progress logs", () => {
  it("keeps per-feature logs isolated", () => {
    useUiStore.getState().appendProgressLog("repo_index", "step A");
    useUiStore.getState().appendProgressLog("repo_index", "step B");
    useUiStore.getState().appendProgressLog("upload_documents", "upload requested");

    const st = useUiStore.getState();
    expect(st.progressLogByFeature.repo_index.map((x) => x.text)).toEqual(["step A", "step B"]);
    expect(st.progressLogByFeature.upload_documents.map((x) => x.text)).toEqual(["upload requested"]);
    expect(st.progressLogByFeature.cache_embeddings).toEqual([]);
  });
});


