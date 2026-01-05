import * as React from "react";
import { cva, type VariantProps } from "class-variance-authority";

import { cn } from "../../lib/cn";

const badgeVariants = cva(
  "inline-flex items-center rounded-full border px-2.5 py-0.5 text-xs font-medium leading-5",
  {
    variants: {
      variant: {
        default: "border-border bg-secondary text-secondary-foreground",
        outline: "border-border bg-transparent text-foreground",
        success: "border-emerald-500/25 bg-emerald-500/10 text-emerald-700 dark:text-emerald-300",
        warning: "border-amber-500/25 bg-amber-500/10 text-amber-700 dark:text-amber-300",
        destructive:
          "border-destructive/25 bg-destructive/10 text-destructive dark:text-destructive-foreground",
      },
    },
    defaultVariants: { variant: "default" },
  },
);

export function Badge(
  props: React.HTMLAttributes<HTMLSpanElement> & VariantProps<typeof badgeVariants>,
) {
  const { className, variant, ...rest } = props;
  return <span className={cn(badgeVariants({ variant }), className)} {...rest} />;
}



