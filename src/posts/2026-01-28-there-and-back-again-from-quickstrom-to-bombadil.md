---
title: "There and Back Again: From Quickstrom to Bombadil"
date: "January 28, 2026"
---

Today I'm announcing and open-sourcing the
[Bombadil](https://github.com/antithesishq/bombadil) project --- a brand new
property-based browser testing framework. I started working on this when
joining Antithesis two months ago. While still in its infancy, we've decided to
build it in the open and share our progress from the start.


We decided on the name Bombadil last week. A few days later, this exploded:

<blockquote class="twitter-tweet"><p lang="en" dir="ltr">It&#39;s going to be tough for startups when all the Lord of the Rings names are taken and the only thing left is something like Bombadil AI.</p>&mdash; Patrick Collison (@patrickc) <a href="https://twitter.com/patrickc/status/2015562569105465347?ref_src=twsrc%5Etfw">January 25, 2026</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script> 

While [Quickstrom](https://github.com/quickstrom/quickstrom) proved its worth,
[finding bugs in more than half of the
TodoMVC](https://dl.acm.org/doi/10.1145/3519939.3523728) apps, Bombadil aims to
improve on its shortcomings while also envisioning a more ambitious future for
generative testing of web apps: faster and smarter state space exploration, a
modern and usable specification language, better tools for reproducing and
debugging test failures, and a better distribution story.

I consider Bombadil the successor of Quickstrom. After years of trying to
sustain Quickstrom through various models, I now have a much better answer:
building it at Antithesis, making something valuable that is open and free to
use, while also strengthening the company's commercial offering. Bombadil can
be used locally or in CI to test your web apps early. Power users can take it
further and run Bombadil within Antithesis and its deterministic hypervisor.
That gives you perfect reproductions of failed tests. You can even combine
Bombadil with other workloads in Antithesis and test your entire stack
deterministically. Isn't that the holy grail of testing?

Bombadil is built from scratch with a focus on accessibility: a new
specification language and better tooling for writing and maintaining specs.
Right now, I'm working on a specification DSL in TypeScript. It's based on
[linear temporal logic](https://en.wikipedia.org/wiki/Linear_temporal_logic),
just as Quickstrom, but aims to be a lot more ergonomic. Here's an example that
verifies that error messages eventually disappear:

```typescript
const errorMessage = extract(
  (state) =>
    state.document.body.querySelector(".error")
        ?.textContent 
        ?? null
);

export const errorEventuallyDisappears = always(
  condition(() => errorMessage !== null).implies(
    eventually(() => errorMessage === null)
        .within(5, "seconds")
  )
);
```

Both the original hacky PureScript DSL and the bespoke language *Specstrom*
were huge obstacles to adoption. Today, TypeScript is a widely adopted
language among quality-minded web developers, and if you're not into that,
Bombadil will work with plain JavaScript too.

We're writing Bombadil in Rust, leveraging its excellent ecosystem to ship a
single statically-linked executable --- download for your platform, point it at
a Chromium-based browser, and off you go!

Bombadil is early but usable. Check it out on
[GitHub](https://github.com/antithesishq/bombadil), try it on your projects,
and let us know what breaks. Also let us know what *it* breaks in your systems!
Join us on [Discord](https://discord.gg/antithesis) for help and discussion, or
follow development on [Twitter/X](https://x.com/owickstrom). This is just the
beginning --- we're actively seeking feedback and early adopters to help shape
where this goes.
