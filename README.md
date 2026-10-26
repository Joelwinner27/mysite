# joelwinner.org

Personal portfolio and sports analytics blog, built with [Astro](https://astro.build) and Tailwind, deployed on Netlify.

- `src/content/projects/*.md`: one file per project (frontmatter drives the cards)
- `src/content/posts/*.md`: blog posts
- `public/`: résumé PDF, images, and figures rendered from R
- `analysis/`: the original R Markdown sources; knit them and copy the output into `src/content` and `public/`

```bash
npm install
npm run dev      # http://localhost:4321
npm run build    # outputs to dist/
```
