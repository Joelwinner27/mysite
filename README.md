# joelwinner.org

Résumé, sports analytics write ups and projects. Built with [Astro](https://astro.build) and Tailwind, deployed on Netlify.

- `src/content/analysis/*.md`: posts. `type` is `quick-hit`, `analysis` or `deep-dive`
- `src/content/projects/*.md`: things I've built
- `src/pages/resume.astro`: the web résumé (keep it in sync with the PDF in `public/`)
- `public/`: résumé PDF, images, and charts rendered from R
- `analysis/`: the original R Markdown sources
- `docs/quick-hit-template.md`: copy this to start a new post

## Posting a quick hit

1. Make the chart in R and save it to `public/analysis/<slug>/chart.png`.
2. Copy `docs/quick-hit-template.md` to `src/content/analysis/<slug>.md` and fill it in.
3. `npm run dev` to check it at http://localhost:4321, then commit and push.

Every post gets a share image for X automatically (`/og/analysis/<slug>.png`).

```bash
npm install
npm run dev      # http://localhost:4321
npm run build    # outputs to dist/
```
