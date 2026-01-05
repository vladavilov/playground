import React from "react";
import { describe, expect, it } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";
import { createMemoryRouter, Link, Outlet, RouterProvider } from "react-router";

import { useUiStore } from "./store";

function Page(props: { routeKey: string }) {
  const value = useUiStore((s) => s.chatDraftByRouteKey[props.routeKey] ?? "");
  const setChatDraft = useUiStore((s) => s.setChatDraft);

  return (
    <div>
      <label htmlFor="draft">draft</label>
      <input
        id="draft"
        value={value}
        onChange={(e) => setChatDraft(props.routeKey, e.target.value)}
      />
      <div data-testid="value">{value}</div>
      <Link to="/b">to-b</Link>
    </div>
  );
}

function Layout() {
  return <Outlet />;
}

describe("store integration", () => {
  it("persists drafts across navigation without loaders", async () => {
    useUiStore.getState().reset();

    const router = createMemoryRouter(
      [
        {
          path: "/",
          element: <Layout />,
          children: [
            { index: true, element: <Page routeKey="a" /> },
            { path: "b", element: <Page routeKey="a" /> },
          ],
        },
      ],
      { initialEntries: ["/"] },
    );

    render(<RouterProvider router={router} />);

    fireEvent.change(screen.getByLabelText("draft"), {
      target: { value: "hello" },
    });
    expect(screen.getByTestId("value").textContent).toBe("hello");

    fireEvent.click(screen.getByText("to-b"));
    expect(await screen.findByTestId("value")).toHaveTextContent("hello");
  });
});



