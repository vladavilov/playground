import React from "react";
import { describe, expect, it } from "vitest";
import { fireEvent, render, screen, waitFor } from "@testing-library/react";

import { ChatShell } from "./ChatShell";
import type { ChatMessage } from "./chat.types";

describe("ChatShell", () => {
  it("collapses chat into a narrow rail that shows the first user message (and can restore)", async () => {
    const messages: ChatMessage[] = [
      { id: "s1", kind: "text", role: "system", content: "hello", createdAt: 0 },
      { id: "u1", kind: "text", role: "user", content: "First user message", createdAt: 1 },
      { id: "u2", kind: "text", role: "user", content: "Second user message", createdAt: 2 },
    ];

    render(
      <div className="h-[600px]">
        <ChatShell
          messages={messages}
          input=""
          setInput={() => {}}
          onSend={() => {}}
          rightPanel={<div>Right</div>}
        />
      </div>,
    );

    expect(screen.getByRole("button", { name: "Collapse chat" })).toBeTruthy();
    fireEvent.click(screen.getByRole("button", { name: "Collapse chat" }));

    expect(screen.getByRole("button", { name: "Restore chat" })).toBeTruthy();
    expect(screen.getAllByText("First user message").length).toBeGreaterThan(0);
    await waitFor(() => {
      expect(screen.queryByText("Second user message")).toBeNull();
    });

    fireEvent.click(screen.getByRole("button", { name: "Restore chat" }));
    expect(screen.getByRole("button", { name: "Collapse chat" })).toBeTruthy();
  });
});


