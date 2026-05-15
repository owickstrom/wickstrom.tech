import { not, eventually, extract } from "@antithesishq/bombadil";
export * from "@antithesishq/bombadil/defaults";

function splitWords(text: string): string[] {
  return text.split(/[\s.?!]/)
}

const word_count = extract(state =>
  [...state.document.querySelectorAll("body > p")].flatMap(p => splitWords(p.textContent)).length
);

const WORD_COUNT_MAX = 3000;

export const no_huge_articles = not(eventually(() => word_count.current > WORD_COUNT_MAX));
