import NSpell from "nspell";
import { extract, always } from "@antithesishq/bombadil";
import { getSpellCheckableWords } from "./wordExtractor.ts";
export * from "@antithesishq/bombadil/defaults";

// @ts-ignore
import affUs from "./node_modules/dictionary-en/index.aff" with { type: "text" };
// @ts-ignore
import dicUs from "./node_modules/dictionary-en/index.dic" with { type: "text" };
// @ts-ignore
import affGb from "./node_modules/dictionary-en-gb/index.aff" with { type: "text" };
// @ts-ignore
import dicGb from "./node_modules/dictionary-en-gb/index.dic" with { type: "text" };
// @ts-ignore
import customDic from "./custom.utf-8.add" with { type: "text" };

const misspelled = extract(state => {
  const spell_us = new NSpell(affUs, dicUs).personal(customDic);
  const spell_gb = new NSpell(affGb, dicGb).personal(customDic);

  const body = state.document.querySelector("body");
  if (!body) { return []; }

  const ct = state.document.contentType || "";
  if (!ct.includes("html")) { return []; }

  const words = [...getSpellCheckableWords(body)];

  return words
    .filter(word => !/^[A-Z]/.test(word))
    .filter(word => !spell_us.correct(word) && !spell_gb.correct(word));
});

export const no_spelling_errors = always(() => {
  return misspelled.current.length === 0
});
