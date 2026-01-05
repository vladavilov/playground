import type { Route } from "./+types/_index";
import { redirect } from "react-router";

export async function loader(_: Route.LoaderArgs) {
  throw redirect("/projects");
}

export default function Index() {
  return null;
}



