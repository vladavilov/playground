import type { ReactNode } from "react";
import { motion } from "motion/react";

import { cn } from "../lib/cn";
import { motionDuration } from "../lib/motion";

export function Page(props: { children: ReactNode; className?: string }) {
  return (
    <motion.main
      className={cn("flex min-h-0 flex-1 flex-col", props.className)}
      initial={{ opacity: 0, y: 6 }}
      animate={{ opacity: 1, y: 0 }}
      exit={{ opacity: 0, y: 6 }}
      transition={{ duration: motionDuration(0.18), ease: "easeOut" }}
    >
      {props.children}
    </motion.main>
  );
}



