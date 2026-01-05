import * as React from "react";

import { cn } from "../../lib/cn";

export function Card(props: React.HTMLAttributes<HTMLDivElement>) {
  return (
    <div
      className={cn(
        "rounded-lg border border-border bg-card text-card-foreground shadow-sm transition-shadow",
        props.className,
      )}
      {...props}
    />
  );
}

export function CardHeader(props: React.HTMLAttributes<HTMLDivElement>) {
  return (
    <div className={cn("flex items-center justify-between gap-3 p-4", props.className)} {...props} />
  );
}

export function CardTitle(props: React.HTMLAttributes<HTMLHeadingElement>) {
  return (
    <h3 className={cn("text-sm font-semibold leading-none tracking-tight", props.className)} {...props} />
  );
}

export function CardDescription(props: React.HTMLAttributes<HTMLParagraphElement>) {
  return (
    <p className={cn("text-sm text-muted-foreground", props.className)} {...props} />
  );
}

export function CardContent(props: React.HTMLAttributes<HTMLDivElement>) {
  return <div className={cn("p-4 pt-0", props.className)} {...props} />;
}


