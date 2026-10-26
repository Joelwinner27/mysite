export const site = {
  name: "Joel Winner",
  title: "Joel Winner · Sports analytics and data projects",
  description:
    "Joel Winner is an Ohio State statistics graduate who builds sports analytics models, data visualizations and data tools.",
  email: "Joelwinner.127@gmail.com",
  resume: "/Joel-Resume.pdf",
  links: {
    linkedin: "https://www.linkedin.com/in/joel-winner/",
    github: "https://github.com/Joelwinner27",
    tableau: "https://public.tableau.com/app/profile/joel.winner",
  },
};

export const nav = [
  { label: "Projects", href: "/projects/" },
  { label: "Blog", href: "/posts/" },
  { label: "About", href: "/#about" },
];

export const formatDate = (d: Date) =>
  d.toLocaleDateString("en-US", { month: "short", day: "numeric", year: "numeric", timeZone: "UTC" });
