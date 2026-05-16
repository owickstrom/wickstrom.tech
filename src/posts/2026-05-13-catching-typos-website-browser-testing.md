---
title: "Catching Typos on My Website with Browser Testing"
date: "May 13, 2026"
author: "Oskar Wickström"
---

One neat thing about [Bombadil's specification
language](https://antithesishq.github.io/bombadil/3-specification-language.html#specification-language)
is that it's plain TypeScript, with access to external NPM packages. I've
written a specification that spell-checks my website --- what you're reading
now --- and I want to share how that turned out.

The inner loop (spell-checking):

: Bombadil randomly walks the website and collects misspelled words as property
violations. The specification uses
[nspell](https://github.com/wooorm/nspell){spellcheck=false} with [American and
British English
dictionaries](https://github.com/wooorm/dictionaries/tree/main/dictionaries/en)
and a personal word list in the repository. This is _fast_ and _strict_.

The outer loop (triage):

: I'm running Claude Code with a spell-checking skill, a triage loop that
    goes something like this:

    1. Run Bombadil against the local development server for 5 minutes and
       capture the output. If no words flagged, we're done.
    2. Collect each flagged word and the URL it appeared on.
    3. Triage each word into one of these buckets:
        * Real typo: fix the markdown source
        * Legitimate common word: add to the custom dictionary
        * Legitimate uncommon or very technical word: mark inline with
          `spellcheck="false"`
        * Extraction noise: add a unit test and fix the word extractor
    4. Run Bombadil against each failing URL to confirm the corrections.
    5. Go to step 1.

    This is _slow_ and _loose_.

The hybrid model seems to work well; it has flagged words in almost every blog
post. It has fixed 13 real typos and added 130+ words to my personal
dictionary. <span spellcheck=false>Example typos include "forseeable", "similiar", "perculiar",
"occured". Some of these were 10 years old.</span>

Claude doesn't have to waste tokens spell-checking everything over and over.
Right now I'm just running this locally, but you could imagine a more elaborate
setup for large websites where the "inner loop" runs as a nightly job, invoking
the "outer loop" only on violations. You could involve a human where needed,
and build up a domain-specific dictionary over time.

Note that using an LLM is entirely optional. It just saves me some time. You
can do triage on your own.

Why not spell-check the sources directly? Yes, that is often preferable, and I
use `spell` in Neovim all the time. But it's not always practical. At least in
my experience, the tooling trips up on syntax and templating in more
complicated setups. Maybe your editor handles this better than mine does, or
maybe you're fine with tools like [typos](https://crates.io/crates/typos-cli)
and
[codespell](https://github.com/codespell-project/codespell){spellcheck=false},
but I like the fact that this approach is external and checks the rendered
output. Given that Bombadil interacts with web applications, you could even run
this against dynamic applications to spell-check states deep in the UI.

Speaking of source-level checking: since the custom dictionary is a plain
word list, I point Neovim's `spellfile` at it and use `zg` to add words while I
edit. A single source of truth that both tools write to.

```lua
vim.opt.spellfile = "/path/to/custom.utf-8.add"
vim.opt.spelllang = "en"
```

Being able to use NPM packages in specifications has turned out to be more
useful than I expected. In addition to `nspell`, I'm using
[tlds](https://www.npmjs.com/package/tlds){spellcheck=false} to identify URLs.
Bombadil is built for property-based testing of web applications, but with a
specification language and package ecosystem at hand, its uses might be broader
than my original vision.

If you're interested in setting up something like this on your own, you'll find
the sources in [my Bombadil
playground](https://github.com/owickstrom/bombadil-playground/tree/master/wickstrom.tech).

*Disclosure: I'm the original author and lead for the Bombadil project at Antithesis.*
