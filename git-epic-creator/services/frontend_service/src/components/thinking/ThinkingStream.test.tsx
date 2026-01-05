import React from "react";
import { afterEach, describe, expect, it } from "vitest";
import { fireEvent, render, screen, waitFor } from "@testing-library/react";

import { useUiStore } from "../../state/store";
import { ThinkingStream } from "./ThinkingStream";

afterEach(() => {
  useUiStore.getState().reset();
});

describe("ThinkingStream", () => {
  it("renders grouped by prompt_id and can collapse", async () => {
    useUiStore.getState().appendThinking("p1", "hello");
    useUiStore.getState().appendThinking("p1", "world");

    render(<ThinkingStream />);
    expect(screen.getByText("prompt_id: p1")).toBeInTheDocument();
    expect(screen.getByText("hello")).toBeInTheDocument();

    fireEvent.click(screen.getByText("Collapse"));
    await waitFor(() => {
      expect(screen.queryByText("hello")).toBeNull();
    });
  });
});



