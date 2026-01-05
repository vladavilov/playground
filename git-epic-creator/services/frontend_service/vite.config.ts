import { defineConfig } from "vite";
import tailwindcss from "@tailwindcss/vite";
import { reactRouter } from "@react-router/dev/vite";

// https://vitejs.dev/config/
export default defineConfig({
  optimizeDeps: {
    // MSW has Node-only subpath exports that break Vite dep optimization when a module graph
    // references them (even behind runtime checks). We tree-shake them with `import.meta.env.SSR`,
    // but exclude defensively to avoid dev server crashes.
    exclude: [
      "msw",
      "msw/browser",
      "msw/node",
      "@mswjs/interceptors",
      "@mswjs/interceptors/ClientRequest",
    ],
  },
  server: {
    port: 3000,
    proxy: Object.fromEntries(
      [
        "/auth",
        // NOTE: must not match the UI route `/projects/*`
        "/project/",
        "/workflow",
        "/tasks",
        "/gitlab",
        "/events",
      ].map((path) => [
        path,
        {
          target: process.env.SSE_BRIDGE_ORIGIN ?? "http://localhost:8007",
          changeOrigin: true,
          secure: false,
        },
      ]),
    ),
  },
  plugins: [reactRouter(), tailwindcss()],
});
