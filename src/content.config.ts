import { defineCollection, z } from "astro:content";
import { glob } from "astro/loaders";

// Keep file names as ids so URLs match the old Hugo site (e.g. kenpom-11.13).
const keepName = ({ entry }: { entry: string }) => entry.replace(/\.md$/, "");

const projects = defineCollection({
  loader: glob({ pattern: "*.md", base: "./src/content/projects", generateId: keepName }),
  schema: z.object({
    title: z.string(),
    // Newspaper-style headline for cards and the article page; falls back to title.
    headline: z.string().optional(),
    date: z.coerce.date(),
    sport: z.string(),
    summary: z.string(),
    tools: z.array(z.string()),
    thumbnail: z.string().optional(),
    featured: z.boolean().default(false),
    live: z.boolean().default(false),
    order: z.number().default(99),
  }),
});

const posts = defineCollection({
  loader: glob({ pattern: "*.md", base: "./src/content/posts", generateId: keepName }),
  schema: z.object({
    title: z.string(),
    date: z.coerce.date(),
    tags: z.array(z.string()).default([]),
  }),
});

export const collections = { projects, posts };
