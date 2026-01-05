import { describe, expect, it } from "vitest";
import { render, screen } from "@testing-library/react";
import { createMemoryRouter, RouterProvider } from "react-router";

import Root from "./root";

describe("root", () => {
  it("renders with a minimal router harness", () => {
    const router = createMemoryRouter(
      [
        {
          path: "/",
          element: <Root />,
          children: [{ index: true, element: <div>ok</div> }],
        },
      ],
      { initialEntries: ["/"] },
    );

    render(<RouterProvider router={router} />);
    expect(screen.getByText("ok")).toBeInTheDocument();
  });
});



