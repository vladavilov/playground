import type { ReactNode } from "react";

import { cn } from "../../lib/cn";

type Variant = "neutral" | "success" | "warning" | "danger";

const VARIANT: Record<Variant, string> = {
  neutral: "border-border bg-secondary text-secondary-foreground",
  success: "border-emerald-500/25 bg-emerald-500/10 text-emerald-700 dark:text-emerald-300",
  warning: "border-amber-500/25 bg-amber-500/10 text-amber-700 dark:text-amber-300",
  danger: "border-rose-500/25 bg-rose-500/10 text-rose-700 dark:text-rose-300",
};

export function StatusPill(props: {
  variant: Variant;
  icon?: ReactNode;
  children: ReactNode;
  onClick?: () => void;
  href?: string;
  title?: string;
  disabled?: boolean;
}) {
  const base = cn(
    "inline-flex items-center gap-2 rounded-full border px-3 py-1 text-xs font-medium",
    VARIANT[props.variant],
    (props.onClick || props.href) && !props.disabled && "cursor-pointer hover:bg-accent/50",
    props.disabled && "opacity-60",
  );

  if (props.href) {
    return (
      <a
        href={props.href}
        title={props.title}
        className={base}
        onClick={props.disabled ? (e) => e.preventDefault() : undefined}
      >
        {props.icon}
        {props.children}
      </a>
    );
  }

  if (props.onClick) {
    return (
      <button
        type="button"
        title={props.title}
        onClick={props.onClick}
        className={base}
        disabled={props.disabled}
      >
        {props.icon}
        {props.children}
      </button>
    );
  }

  return (
    <span title={props.title} className={base}>
      {props.icon}
      {props.children}
    </span>
  );
}



