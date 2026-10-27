import type { CollectionEntry } from "astro:content";

export const site = {
  name: "Joel Winner",
  title: "Joel Winner · Sports analytics and data work",
  description:
    "Joel Winner is an analyst and Ohio State statistics graduate. Sports analytics write ups, quick hits after big games, projects and résumé.",
  email: "Joelwinner.127@gmail.com",
  resume: "/Joel-Resume.pdf",
  // X handle without the @. Leave empty to hide the follow button.
  x: "",
  links: {
    linkedin: "https://www.linkedin.com/in/joel-winner/",
    github: "https://github.com/Joelwinner27",
    tableau: "https://public.tableau.com/app/profile/joel.winner",
  },
};

export const nav = [
  { label: "Analysis", href: "/analysis/" },
  { label: "Projects", href: "/projects/" },
  { label: "Résumé", href: "/resume/" },
];

export const typeLabel: Record<CollectionEntry<"analysis">["data"]["type"], string> = {
  "quick-hit": "Quick hit",
  analysis: "Analysis",
  "deep-dive": "Deep dive",
};

export const byNewest = <T extends { data: { date: Date } }>(a: T, b: T) => b.data.date.valueOf() - a.data.date.valueOf();

export const readingTime = (body = "") => {
  // Skip code, embedded styles and scripts, and tags (rendered R output carries a lot of each).
  const prose = body
    .replace(/```[\s\S]*?```/g, "")
    .replace(/<(style|script)[\s\S]*?<\/\1>/g, "")
    .replace(/<[^>]+>/g, " ");
  const words = prose.split(/\s+/).filter(Boolean).length;
  return `${Math.max(1, Math.round(words / 230))} min read`;
};

export const formatDate = (d: Date) =>
  d.toLocaleDateString("en-US", { month: "short", day: "numeric", year: "numeric", timeZone: "UTC" });

export const shortDate = (d: Date) => d.toLocaleDateString("en-US", { month: "short", day: "numeric", timeZone: "UTC" });
