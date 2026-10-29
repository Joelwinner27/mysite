import { defineCollection, z } from "astro:content";
import { glob } from "astro/loaders";

// Keep file names as ids so URLs match the old Hugo site (e.g. kenpom-11.13).
const keepName = ({ entry }: { entry: string }) => entry.replace(/\.md$/, "");

const analysis = defineCollection({
  loader: glob({ pattern: "*.md", base: "./src/content/analysis", generateId: keepName }),
  schema: z.object({
    title: z.string(),
    // quick-hit: one chart, same night. analysis: a fuller take. deep-dive: a full study.
    type: z.enum(["quick-hit", "analysis", "deep-dive"]),
    sport: z.string(),
    date: z.coerce.date(),
    summary: z.string(),
    // "The bottom line" box at the top of the post.
    stats: z.array(z.object({ value: z.string(), label: z.string() })).optional(),
    // Shown under the bottom line; use it when the body doesn't already open with the chart.
    chart: z.string().optional(),
    chartCaption: z.string().optional(),
    thumbnail: z.string().optional(),
    data: z.string().optional(),
    code: z.string().url().optional(),
    tools: z.array(z.string()).default([]),
  }),
});

const projects = defineCollection({
  loader: glob({ pattern: "*.md", base: "./src/content/projects", generateId: keepName }),
  schema: z.object({
    title: z.string(),
    date: z.coerce.date(),
    sport: z.string(),
    summary: z.string(),
    tools: z.array(z.string()),
    thumbnail: z.string().optional(),
    live: z.boolean().default(false),
    order: z.number().default(99),
  }),
});

export const collections = { analysis, projects };
