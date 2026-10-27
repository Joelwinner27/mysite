// Share images (1200×630) for X and LinkedIn, rendered at build time with satori.
import type { APIRoute, GetStaticPaths } from "astro";
import { getCollection } from "astro:content";
import { readFile } from "node:fs/promises";
import satori from "satori";
import { Resvg } from "@resvg/resvg-js";
import { typeLabel } from "../../data/site";

const font = (pkg: string, file: string) => readFile(`node_modules/@fontsource/${pkg}/files/${file}`);
const fonts = Promise.all([
  font("playfair-display", "playfair-display-latin-800-normal.woff"),
  font("ibm-plex-mono", "ibm-plex-mono-latin-500-normal.woff"),
  font("source-serif-4", "source-serif-4-latin-400-normal.woff"),
]);

interface Card {
  kicker: string;
  title: string;
  summary: string;
  image?: string;
}

export const getStaticPaths: GetStaticPaths = async () => {
  const analysis = await getCollection("analysis");
  const projects = await getCollection("projects");
  return [
    {
      params: { route: "home" },
      props: {
        kicker: "Cincinnati, Ohio · Ohio State Statistics ’25",
        title: "Analyst by day.\nSports nerd by night.",
        summary: "Sports analytics write ups, quick hits after big games, projects and résumé.",
      },
    },
    ...analysis.map((p) => ({
      params: { route: `analysis/${p.id}` },
      props: { kicker: `${typeLabel[p.data.type]} · ${p.data.sport}`, title: p.data.title, summary: p.data.summary, image: p.data.chart ?? p.data.thumbnail },
    })),
    ...projects.map((p) => ({
      params: { route: `projects/${p.id}` },
      props: { kicker: `Project · ${p.data.sport}`, title: p.data.title, summary: p.data.summary, image: p.data.thumbnail },
    })),
  ];
};

const el = (type: string, style: Record<string, unknown>, children?: unknown, extra: Record<string, unknown> = {}) => ({
  type,
  props: { style, children, ...extra },
});

export const GET: APIRoute = async ({ props }) => {
  const { kicker, title, summary, image } = props as Card;
  const [playfair, mono, serif] = await fonts;
  const src = image ? `data:image/png;base64,${(await readFile(`public${image}`)).toString("base64")}` : undefined;

  const text = el("div", { display: "flex", flexDirection: "column", flex: 1, gap: 24 }, [
    el("div", { fontFamily: "Plex Mono", fontSize: 22, color: "#a3322a", letterSpacing: 2, textTransform: "uppercase" }, kicker),
    el("div", { fontFamily: "Playfair", fontSize: src ? 60 : 76, lineHeight: 1.05, color: "#231c14", whiteSpace: "pre-wrap" }, title),
    el("div", { fontFamily: "Source Serif", fontSize: 28, lineHeight: 1.35, color: "#4a3f31" }, summary),
  ]);
  const chart = src
    ? el("div", { display: "flex", width: 420, height: 420, border: "2px solid #231c14", background: "#f8f2e6", padding: 16, alignItems: "center", justifyContent: "center" }, [
        el("img", { maxWidth: "100%", maxHeight: "100%", objectFit: "contain" }, undefined, { src }),
      ])
    : null;

  const tree = el("div", { display: "flex", flexDirection: "column", width: 1200, height: 630, background: "#f3ebdc", padding: 56 }, [
    el("div", { display: "flex", flex: 1, gap: 48, alignItems: "center" }, chart ? [text, chart] : [text]),
    el("div", { display: "flex", justifyContent: "space-between", borderTop: "3px solid #231c14", paddingTop: 20, fontFamily: "Plex Mono", fontSize: 22, color: "#231c14", letterSpacing: 2, textTransform: "uppercase" }, [
      el("div", {}, "Joel Winner"),
      el("div", {}, "joelwinner.org"),
    ]),
  ]);

  const svg = await satori(tree as never, {
    width: 1200,
    height: 630,
    fonts: [
      { name: "Playfair", data: playfair, weight: 800, style: "normal" },
      { name: "Plex Mono", data: mono, weight: 500, style: "normal" },
      { name: "Source Serif", data: serif, weight: 400, style: "normal" },
    ],
  });
  const png = new Resvg(svg, { fitTo: { mode: "width", value: 1200 } }).render().asPng();
  return new Response(png, { headers: { "Content-Type": "image/png" } });
};
