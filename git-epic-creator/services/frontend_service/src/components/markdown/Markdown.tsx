import ReactMarkdown from "react-markdown";
import remarkBreaks from "remark-breaks";
import remarkGfm from "remark-gfm";
import rehypeSanitize from "rehype-sanitize";

import { Mermaid } from "./Mermaid";

export function Markdown(props: { markdown: string }) {
  return (
    <ReactMarkdown
      remarkPlugins={[remarkGfm, remarkBreaks]}
      rehypePlugins={[rehypeSanitize]}
      components={{
        pre({ children }) {
          return (
            <pre className="overflow-auto rounded-md border border-border bg-muted p-3 text-xs">
              {children}
            </pre>
          );
        },
        code({ className, children }) {
          const lang = (className ?? "").replace("language-", "").trim();
          const code = String(children ?? "");

          if (lang === "mermaid") {
            return <Mermaid code={code} />;
          }

          return <code className={className}>{children}</code>;
        },
      }}
    >
      {props.markdown}
    </ReactMarkdown>
  );
}


