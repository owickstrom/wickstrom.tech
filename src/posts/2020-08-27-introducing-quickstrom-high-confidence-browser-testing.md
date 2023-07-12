---
title: "Introducing Quickstrom: High-confidence browser testing"
author: Oskar Wickström
date: August 27, 2020
---

In [the last
post](https://wickstrom.tech/programming/2020/07/02/the-todomvc-showdown-testing-with-webcheck.html)
I shared the results from testing TodoMVC implementations using WebCheck. The
project has since been renamed _Quickstrom_ (thank you, [Tom](https://twitter.com/am_i_tom/)) and is now released as open
source.

## What is Quickstrom?

Quickstrom is a new autonomous testing tool for the web. It can find problems
in any type of web application that renders to the DOM. Quickstrom
automatically explores your application and presents minimal failing
examples. Focus your effort on understanding and specifying your system, and
Quickstrom can test it for you.

## Past and future

I started writing Quickstrom on April 2, 2020, about a week after our first
child was born. Somehow that code compiled, and evolved into a capable
testing tool. I'm now happy and excited to share it with everyone!

In the future, when Quickstrom is more robust and has a greater mileage, I
might build a commercial product on top of it. This one of the reasons I've
chosen an AGPL-2.0 license for the code, and why contributors must sign a CLA
before pull requests can be merged. The idea is to keep the CLI test runner
AGPL forever, but I might need a license exception if I build a closed-source
SaaS product later on.

## Learning more

Interested in Quickstrom? Start by checking out any of these resources:

* [Main website](https://quickstrom.io)
* [Project documentation](https://docs.quickstrom.io), including installation instructions and usage guides
* [Source code](https://github.com/quickstrom/quickstrom)

And keep an eye out for updates by signing up for [the
newsletter](https://buttondown.email/quickstrom), or by following [me on
Twitter](https://twitter.com/owickstrom). Documentation should be
significantly improved soon.

## Comments

If you have any comments or questions, please reply to any of the following
threads:

- [Twitter](https://twitter.com/owickstrom/status/1299064145736798208)
- [Lobste.rs](https://lobste.rs/s/zrusmd/introducing_quickstrom_high_confidence)