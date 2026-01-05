import { cleanup, fireEvent, render, screen } from "@testing-library/react";
import { afterEach, describe, expect, it, vi } from "vitest";

import { BacklogEditor } from "./BacklogEditor";

import React from "react";
import { buildApplyBacklogRequest } from "../../routes/tasks.submit";

describe("BacklogEditor deletion", () => {
  afterEach(() => cleanup());

  it("deletes an epic via the preview icon button", () => {
    const onChange = vi.fn();
    render(
      <BacklogEditor
        backlog={{
          prompt_id: "pr",
          project_id: "p",
          score: 1,
          assumptions: [],
          risks: [],
          epics: [{ id: "e1", title: "E1", description: "d", tasks: [] }],
        }}
        gitlabProjectIds={[]}
        onChange={onChange}
        onEnhance={() => {}}
      />,
    );

    fireEvent.click(screen.getByLabelText("Delete epic: E1"));
    expect(onChange).toHaveBeenCalledTimes(1);
    const next = onChange.mock.calls[0]?.[0] as { epics: Array<{ id: string }> };
    expect(next.epics).toEqual([]);
  });

  it("deletes a task via the preview icon button", () => {
    const onChange = vi.fn();
    render(
      <BacklogEditor
        backlog={{
          prompt_id: "pr",
          project_id: "p",
          score: 1,
          assumptions: [],
          risks: [],
          epics: [
            {
              id: "e1",
              title: "E1",
              description: "d",
              tasks: [{ id: "t1", title: "T1", description: "td", acceptance_criteria: [], dependencies: [] }],
            },
          ],
        }}
        gitlabProjectIds={[]}
        onChange={onChange}
        onEnhance={() => {}}
      />,
    );

    fireEvent.click(screen.getByLabelText("Delete task: T1"));
    expect(onChange).toHaveBeenCalledTimes(1);
    const next = onChange.mock.calls[0]?.[0] as { epics: Array<{ tasks: Array<{ id: string }> }> };
    expect(next.epics[0]?.tasks).toEqual([]);
  });
});

describe("BacklogEditor", () => {
  afterEach(() => cleanup());

  it("accepts multiple similar matches and payload builder includes related_to_iids", () => {
    const backlog: any = {
      prompt_id: "pr",
      project_id: "p",
      score: 1,
      assumptions: [],
      risks: [],
      epics: [
        {
          id: "e1",
          title: "E1",
          description: "d",
          tasks: [],
          similar: [
            { kind: "epic", id: "m1", iid: "1", title: "S1", project_id: "100" },
            { kind: "epic", id: "m2", iid: "2", title: "S2", project_id: "100" },
          ],
        },
      ],
    };

    let next: any = backlog;
    function Wrapper() {
      const [st, setSt] = React.useState(backlog);
      next = st;
      return (
        <BacklogEditor
          backlog={st}
          gitlabProjectIds={["100"]}
          onChange={setSt}
          onEnhance={() => {}}
        />
      );
    }

    render(<Wrapper />);

    fireEvent.click(screen.getAllByText("Accept")[0]);
    fireEvent.click(screen.getAllByText("Accept")[1]);

    const req = buildApplyBacklogRequest({
      internalProjectId: "p",
      promptId: "pr",
      defaultGitlabProjectId: "100",
      epics: next.epics,
    });

    expect(req.projects[0].epics[0].related_to_iids.sort()).toEqual(["1", "2"]);
  });

  it("focus edit shows live preview and cancel/close behave correctly", async () => {
    const backlog: any = {
      prompt_id: "pr",
      project_id: "p",
      score: 1,
      assumptions: [],
      risks: [],
      epics: [
        {
          id: "e1",
          title: "Epic title",
          description: "Epic old",
          tasks: [
            {
              id: "t1",
              title: "Task title",
              description: "Task old",
              similar: [],
            },
          ],
          similar: [],
        },
      ],
    };

    function Wrapper() {
      const [st, setSt] = React.useState(backlog);
      return (
        <BacklogEditor
          backlog={st}
          gitlabProjectIds={["100"]}
          onChange={setSt}
          onEnhance={() => {}}
        />
      );
    }

    render(<Wrapper />);
    expect(screen.getByText("Epic title")).toBeInTheDocument();

    // Open the first focus edit (Epic)
    fireEvent.click(screen.getAllByRole("button", { name: "Focus edit" })[0]);
    const epicDesc = await screen.findByDisplayValue("Epic old");
    fireEvent.change(epicDesc, { target: { value: "Epic new" } });
    expect(screen.getByText("Preview")).toBeInTheDocument();
    expect(screen.getAllByText("Epic new").length).toBeGreaterThan(0);
    fireEvent.click(screen.getByRole("button", { name: "Cancel" }));
    expect(screen.getByText("Epic old")).toBeInTheDocument();

    // Open again and close (save)
    fireEvent.click(screen.getAllByRole("button", { name: "Focus edit" })[0]);
    const epicDesc2 = await screen.findByDisplayValue("Epic old");
    fireEvent.change(epicDesc2, { target: { value: "Epic saved" } });
    fireEvent.click(screen.getByRole("button", { name: "Save" }));
    expect(screen.getByText("Epic saved")).toBeInTheDocument();

    // Focus edit for Task (second focus edit)
    fireEvent.click(screen.getAllByRole("button", { name: "Focus edit" })[1]);
    const taskDesc = await screen.findByDisplayValue("Task old");
    fireEvent.change(taskDesc, { target: { value: "Task new" } });
    expect(screen.getAllByText("Task new").length).toBeGreaterThan(0);
    fireEvent.click(screen.getByRole("button", { name: "Save" }));
    expect(screen.getByText("Task new")).toBeInTheDocument();
  });
});


