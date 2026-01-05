import { useFetcher } from "react-router";

import type { Theme } from "../lib/theme";
import { Button } from "./ui/button";
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from "./ui/dropdown-menu";

export function ThemeToggle(props: { theme: Theme }) {
  const fetcher = useFetcher();

  const setTheme = (theme: Theme) => {
    fetcher.submit({ theme }, { method: "post", action: "/" });
  };

  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button variant="outline" size="sm" aria-label="Theme">
          Theme: {props.theme}
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent align="end">
        <DropdownMenuItem onSelect={() => setTheme("light")}>Light</DropdownMenuItem>
        <DropdownMenuItem onSelect={() => setTheme("dark")}>Dark</DropdownMenuItem>
        <DropdownMenuSeparator />
        <DropdownMenuItem disabled>
          Persists via cookie (SSR-safe)
        </DropdownMenuItem>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}



