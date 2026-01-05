import React from "react";
import { afterEach, describe, expect, it, vi } from "vitest";
import { cleanup, fireEvent, render, screen } from "@testing-library/react";

import { ChatComposer } from "./ChatComposer";

describe("ChatComposer", () => {
  afterEach(() => cleanup());

  it("calls onSend when clicking send", () => {
    const onSend = vi.fn();
    render(
      <ChatComposer
        value="hello"
        onChange={() => {}}
        onSend={onSend}
        disabled={false}
      />,
    );

    fireEvent.click(screen.getByRole("button", { name: "Send" }));
    expect(onSend).toHaveBeenCalledTimes(1);
  });

  it("calls onSend on Enter (but not on Shift+Enter)", () => {
    const onSend = vi.fn();
    render(
      <ChatComposer
        value="hello"
        onChange={() => {}}
        onSend={onSend}
        disabled={false}
      />,
    );

    const input = screen.getByPlaceholderText("Write a message…");
    fireEvent.keyDown(input, { key: "Enter", shiftKey: true });
    expect(onSend).toHaveBeenCalledTimes(0);

    fireEvent.keyDown(input, { key: "Enter", shiftKey: false });
    expect(onSend).toHaveBeenCalledTimes(1);
  });

  it("does not send when empty or disabled", () => {
    const onSend = vi.fn();
    const { rerender } = render(
      <ChatComposer value="" onChange={() => {}} onSend={onSend} disabled={false} />,
    );

    fireEvent.click(screen.getByRole("button", { name: "Send" }));
    expect(onSend).toHaveBeenCalledTimes(0);

    rerender(<ChatComposer value="hello" onChange={() => {}} onSend={onSend} disabled />);
    fireEvent.click(screen.getByRole("button", { name: "Send" }));
    expect(onSend).toHaveBeenCalledTimes(0);
  });
});


