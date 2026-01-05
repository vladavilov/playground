import React from "react";
import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";

import { ChatThread } from "./ChatThread";
import type { ChatMessage } from "./chat.types";

describe("ChatThread", () => {
  it("renders markdown for system, user, and assistant messages", () => {
    const messages: ChatMessage[] = [
      {
        id: "s1",
        kind: "text",
        role: "system",
        content: "**System**\n\n```json\n{\"ok\":true}\n```",
        createdAt: 0,
      },
      { id: "u1", kind: "text", role: "user", content: "User says **bold**", createdAt: 1 },
      { id: "a1", kind: "text", role: "assistant", content: "Agent says *italic*", createdAt: 2 },
    ];

    render(
      <div className="h-[400px]">
        <ChatThread messages={messages} />
      </div>,
    );

    expect(screen.getByText("System", { selector: "strong" })).toBeTruthy();
    expect(screen.getByText('{"ok":true}')).toBeTruthy();
    expect(screen.getByText("bold", { selector: "strong" })).toBeTruthy();
    expect(screen.getByText("italic", { selector: "em" })).toBeTruthy();
  });
});


