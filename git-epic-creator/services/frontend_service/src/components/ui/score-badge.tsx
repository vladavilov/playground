import { Badge } from "./badge";

function variantFor(score01: number): "success" | "warning" | "destructive" {
  if (score01 >= 0.8) return "success";
  if (score01 >= 0.6) return "warning";
  return "destructive";
}

export function ScoreBadge(props: { score: number }) {
  const pct = Math.round((props.score ?? 0) * 100);
  return <Badge variant={variantFor(props.score ?? 0)}>{pct}%</Badge>;
}


