import { defineConfig } from "astro/config";
import sitemap from "@astrojs/sitemap";
import tailwindcss from "@tailwindcss/vite";

export default defineConfig({
  site: "https://joelwinner.org",
  integrations: [sitemap()],
  devToolbar: { enabled: false },
  vite: { plugins: [tailwindcss()] },
  markdown: {
    shikiConfig: { theme: "github-light" },
  },
});
