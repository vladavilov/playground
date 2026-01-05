import { useEffect, useState } from "react";
import { useFetcher, useRevalidator } from "react-router";

import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogTitle,
  AlertDialogTrigger,
} from "../ui/alert-dialog";
import { Button } from "../ui/button";

export function DeleteProjectButton(props: { projectId: string; name: string }) {
  const fetcher = useFetcher();
  const revalidator = useRevalidator();
  const [open, setOpen] = useState(false);

  const isOk = (v: unknown): v is { ok: true } =>
    Boolean(v && typeof v === "object" && (v as { ok?: unknown }).ok === true);

  useEffect(() => {
    if (fetcher.state !== "idle") return;
    if (!isOk(fetcher.data)) return;
    setOpen(false);
    revalidator.revalidate();
  }, [fetcher.data, fetcher.state, revalidator]);

  return (
    <AlertDialog open={open} onOpenChange={setOpen}>
      <AlertDialogTrigger asChild>
        <Button variant="outline" size="sm">
          Delete
        </Button>
      </AlertDialogTrigger>
      <AlertDialogContent>
        <AlertDialogTitle>Delete project?</AlertDialogTitle>
        <AlertDialogDescription>This will delete “{props.name}”.</AlertDialogDescription>

        <div className="mt-4 flex justify-end gap-2">
          <AlertDialogCancel asChild>
            <Button variant="outline" size="sm">
              Cancel
            </Button>
          </AlertDialogCancel>
          <AlertDialogAction asChild>
            <Button
              size="sm"
              onClick={() =>
                fetcher.submit({ intent: "delete", projectId: props.projectId }, { method: "post" })
              }
            >
              Delete
            </Button>
          </AlertDialogAction>
        </div>
      </AlertDialogContent>
    </AlertDialog>
  );
}


