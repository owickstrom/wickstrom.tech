---
name: blog-spell-check-loop
description: Spell-check the wickstrom.tech blog by iteratively running Bombadil against the local live-reload server, triaging flagged words, and fixing the source posts, dictionary, or extractor until clean.
user_invocable: true
---

## Prerequisites

- The blog must be running locally at `http://localhost:8080` (live-reloading dev server).
- `spellcheck.ts`, `wordExtractor.ts`, and `custom.utf-8.add` live under
  `wickstrom.tech/` (a subdirectory of the bombadil-playground CWD). All
  commands below use paths relative to the project root — do not `cd`.
- Markdown posts to edit live in `~/projects/wickstrom.tech/src/posts/`
  (a separate checkout — not under the playground).
- URL pattern: posts are served at the root, e.g.
  `http://localhost:8080/2020-07-02-the-todomvc-showdown-testing-with-webcheck.html`
  — *not* under `/posts/`, even though the markdown sources live in
  `src/posts/`. The filename (minus `.md`) becomes the slug.

## Loop

Repeat until step 1 finds no flagged words:

### 1. Run the spell check

```bash
mkdir -p .tmp
bombadil test --headless --exit-on-violation --time-limit 5m \
  http://localhost:8080 wickstrom.tech/spellcheck.ts \
  > .tmp/blog-spellcheck.txt 2>&1
```

Exit code 2 = violation found. Exit code 0 / timeout = clean run on the
pages Bombadil explored in 5 minutes (which may not be exhaustive — see
step 3).

Bombadil prints violations directly on stdout — read
`.tmp/blog-spellcheck.txt` to collect the flagged words and the URLs
where they were found.

### 2. Triage each flagged word

Three outcomes. Apply the right one per word, not per run.

**True positive (real typo).** Find the post under
`~/projects/wickstrom.tech/src/posts/` and fix the markdown source. Do
*not* add typos to `custom.utf-8.add`.

**False positive — legitimate word the dictionary doesn't know.** Two
options, choose by context:

- *Add to `custom.utf-8.add`* when the word is reusable across posts (proper
  nouns, recurring jargon, brand names, regional spellings). One word
  per line, no count header.
- *Mark inline with `spellcheck="false"`* when the word is local to one
  post (a one-off code-like token, a stylized name, a quoted handle).
  Prefer adding the attribute to an existing inline element (`<code>`,
  `<span>`, `<a>`); only wrap in a new `<span spellcheck="false">…</span>`
  if no suitable element exists. Block-level wrapping (`<div>`) is a last
  resort.

**False positive — extraction noise.** The token shouldn't have been
spell-checked at all (URL fragment, decoded entity remnant, hash, etc.).
Fix `wordExtractor.ts` rather than polluting the dictionary. After any
changes there:

```bash
npm --prefix wickstrom.tech test
```

Add a unit test covering the new filter case before fixing.

### 3. Verify the fix on the specific failing page

A 5-minute exploration doesn't guarantee Bombadil revisited the page you
just edited. After applying fixes, target each previously-failing URL
directly with a short time limit:

```bash
bombadil test --headless --exit-on-violation --time-limit 10s \
  <failing-url> wickstrom.tech/spellcheck.ts
```

Use 5s for a quick smoke check, 10s if the page is large. If this still
flags the same word, the fix didn't land — re-triage. If it's clean,
move on.

### 4. Loop

Go back to step 1. The full 5-minute run may surface words on pages the
previous run never reached.

You're done when step 1 completes its 5-minute window with no
violations.

## Reporting

At the end, summarize:

- True positives fixed (word -> correction, with file path).
- Words added to `custom.utf-8.add`.
- Inline `spellcheck="false"` additions (file + element).
- Any `wordExtractor.ts` changes (one-line rationale each).

Keep it terse. The user reads the diff.
