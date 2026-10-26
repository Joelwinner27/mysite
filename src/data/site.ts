export const site = {
  name: "Joel Winner",
  paper: "The Winner Almanac",
  title: "The Winner Almanac · Sports analytics by Joel Winner",
  description:
    "Joel Winner is an Ohio State statistics graduate who builds sports analytics models, data visualizations and data tools.",
  email: "Joelwinner.127@gmail.com",
  resume: "/Joel-Resume.pdf",
  founded: 2025,
  links: {
    linkedin: "https://www.linkedin.com/in/joel-winner/",
    github: "https://github.com/Joelwinner27",
    tableau: "https://public.tableau.com/app/profile/joel.winner",
  },
};

export const nav = [
  { label: "Front page", href: "/", mobile: false },
  { label: "Projects", href: "/projects/", mobile: true },
  { label: "Dispatches", href: "/posts/", mobile: true },
  { label: "About", href: "/#about", mobile: true },
];

const roman = (n: number) =>
  [["X", 10], ["IX", 9], ["V", 5], ["IV", 4], ["I", 1]].reduce(
    (out, [sym, val]) => {
      while (n >= (val as number)) {
        out += sym;
        n -= val as number;
      }
      return out;
    },
    "",
  );

// Volume counts years since the site started; the issue number is the project count.
export const issueLine = (projectCount: number, now = new Date()) =>
  `Vol. ${roman(now.getFullYear() - site.founded + 1)} · No. ${String(projectCount).padStart(2, "0")}`;

export const formatDate = (d: Date) =>
  d.toLocaleDateString("en-US", { month: "short", day: "numeric", year: "numeric", timeZone: "UTC" });

export const longDate = (d: Date) =>
  d.toLocaleDateString("en-US", { weekday: "long", month: "long", day: "numeric", year: "numeric", timeZone: "America/New_York" });
