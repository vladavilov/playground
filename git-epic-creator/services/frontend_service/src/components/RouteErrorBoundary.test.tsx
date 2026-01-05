import React from "react";
import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";
import { createMemoryRouter, RouterProvider } from "react-router";

import { RouteErrorBoundary } from "./RouteErrorBoundary";

describe("RouteErrorBoundary", () => {
  it("renders route error response details", async () => {
    const router = createMemoryRouter(
      [
        {
          path: "/",
          loader: async () => {
            throw new Response("nope", { status: 500, statusText: "Upstream" });
          },
          element: <div>ok</div>,
          errorElement: <RouteErrorBoundary />,
        },
      ],
      { initialEntries: ["/"] },
    );

    render(<RouterProvider router={router} />);
    expect(await screen.findByText("Something went wrong")).toBeInTheDocument();
    expect(screen.getByText("500 Upstream")).toBeInTheDocument();
  });
});



