import { cleanup, fireEvent, render, screen, within } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";
import React from "react";

import { RequirementsEditor } from "./RequirementsEditor";

describe("RequirementsEditor deletion", () => {
  afterEach(() => cleanup());

  it("deletes a requirement via the preview icon button", () => {
    const onChange = vi.fn();
    render(
      <RequirementsEditor
        bundle={{
          prompt_id: "pr",
          project_id: "p",
          score: 1,
          assumptions: [],
          risks: [],
          business_requirements: [
            { id: "b1", title: "BR1", description: "bd", acceptance_criteria: [], priority: "Must" },
          ],
          functional_requirements: [],
        }}
        onChange={onChange}
      />,
    );

    fireEvent.click(screen.getByLabelText("Delete requirement: BR1"));
    expect(onChange).toHaveBeenCalledTimes(1);
    const next = onChange.mock.calls[0]?.[0] as { business_requirements: Array<{ id: string }> };
    expect(next.business_requirements).toEqual([]);
  });

  it("deletes a requirement via the focus editor footer button", () => {
    const onChange = vi.fn();
    render(
      <RequirementsEditor
        bundle={{
          prompt_id: "pr",
          project_id: "p",
          score: 1,
          assumptions: [],
          risks: [],
          business_requirements: [],
          functional_requirements: [
            { id: "f1", title: "FR1", description: "fd", acceptance_criteria: [], priority: "Must" },
          ],
        }}
        onChange={onChange}
      />,
    );

    fireEvent.click(screen.getAllByLabelText("Focus edit")[0]);
    const dialog = screen.getByRole("dialog");
    fireEvent.click(within(dialog).getByText("Delete"));

    expect(onChange).toHaveBeenCalledTimes(1);
    const next = onChange.mock.calls[0]?.[0] as { functional_requirements: Array<{ id: string }> };
    expect(next.functional_requirements).toEqual([]);

    // dialog should be closed (content removed)
    expect(screen.queryByRole("dialog")).toBeNull();
  });
});

describe("RequirementsEditor", () => {
  afterEach(() => cleanup());

  it("edits title inline and calls onChange", () => {
    const bundle: any = {
      prompt_id: "pr",
      project_id: "p",
      score: 1,
      assumptions: [],
      risks: [],
      business_requirements: [
        {
          id: "b1",
          title: "Old",
          description: "d",
          acceptance_criteria: ["a"],
          priority: "Must",
        },
      ],
      functional_requirements: [],
    };

    let nextBundle: any | null = null;
    render(<RequirementsEditor bundle={bundle} onChange={(n) => (nextBundle = n)} />);

    fireEvent.click(screen.getByText("Old"));
    const input = screen.getByDisplayValue("Old");
    fireEvent.change(input, { target: { value: "New" } });

    expect(nextBundle.business_requirements[0].title).toBe("New");
  });

  it("focus edit shows live preview and cancel/close behave correctly", async () => {
    const bundle: any = {
      prompt_id: "pr",
      project_id: "p",
      score: 1,
      assumptions: [],
      risks: [],
      business_requirements: [
        {
          id: "b1",
          title: "Title",
          description: "Old desc",
          acceptance_criteria: ["a"],
          priority: "Must",
        },
      ],
      functional_requirements: [],
    };

    function Wrapper() {
      const [st, setSt] = React.useState(bundle);
      return <RequirementsEditor bundle={st} onChange={setSt} onEnhance={() => {}} />;
    }

    render(<Wrapper />);

    // Open focus edit
    fireEvent.click(screen.getAllByRole("button", { name: "Focus edit" })[0]);

    // Edit description draft
    const desc = await screen.findByDisplayValue("Old desc");
    fireEvent.change(desc, { target: { value: "New desc" } });

    // Preview updates live
    expect(screen.getByText("Preview")).toBeInTheDocument();
    expect(screen.getAllByText("New desc").length).toBeGreaterThan(0);

    // Cancel discards changes
    fireEvent.click(screen.getByRole("button", { name: "Cancel" }));
    expect(screen.getByText("Old desc")).toBeInTheDocument();

    // Re-open and Close saves changes
    fireEvent.click(screen.getAllByRole("button", { name: "Focus edit" })[0]);
    const desc2 = await screen.findByDisplayValue("Old desc");
    fireEvent.change(desc2, { target: { value: "Saved desc" } });
    fireEvent.click(screen.getByRole("button", { name: "Save" }));

    expect(screen.getByText("Saved desc")).toBeInTheDocument();
  });
});



