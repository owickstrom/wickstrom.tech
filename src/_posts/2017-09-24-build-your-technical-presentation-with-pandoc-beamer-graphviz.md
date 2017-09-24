---
layout: post
title: Programming Your Technical Presentation
date: 2017-09-24
author: Oskar Wickström
category: programming
tags: ["pandoc", "make", "latex"]
published: false
excerpt: TODO
---

Writing technical presentations that include code samples and diagrams
can be really tedious. In mainstream presentation software, such as
Keynote and PowerPoint, your workflow is likely to manually
copy-and-paste source code from your editor into your slides. If
you're not using the drawing capabilities of your presentation
software, you have to perform similar steps to include diagrams.

In my process of writing a technical presentation, code samples and
diagrams are not written first, and included in the slides at the last
minute -- I work iteratively on slide content, source code, and
diagrams. Having to repeat the time consuming and error prone process
of updating code samples in slides, each time my original source code
changes, breaks my creative flow completely. I also want to have my
source code _compiled and executable_, so that I can be confident it
is correct.

The main features I'm looking for in a technical presentation setup
includes:

* Text-based sources for everything (slides, code samples, diagrams,
  presentation template, styling, and build script)
* The ability to include sections of external source code files into
  slides
* Repeatable and fully-automated builds
* PDF output with and without notes

I'm less interested in:

* Slide transitions and animation
* Videos and GIFs

This article demonstrates a setup that fulfills these goals, using
Pandoc Markdown, Beamer, Graphviz and Make.

## Writing Slides with Pandoc Markdown

One of my favorite tools in technical writing is [Pandoc][]. I use it
for documentation, talks, Markdown preview, this blog post, and for
converting existing documents to more desirable formats[^1].

A very nice feature of Pandoc is slideshow output formats. You can
write your slides in Markdown using regular headings, with the
slide content below:

``` {.markdown include=_posts/pandoc-beamer-examples/first.md}
```

Build the LaTeX source code using Pandoc and the `beamer` target, and
then generate the PDF using `pdflatex`:

``` shell
pandoc -t beamer -o slides.tex slides.md
pdflatex slides.tex
```

Voilà! You have a PDF, such as [this one](/generated/pandoc-beamer-examples/first.pdf).


## Including Source Code from External Files

As stated in the introduction of this pots, I want my source code
samples to compile, and possibly be executable. If I have to write
code directly in the slides, I will most likely make mistakes, and
there will be no compiler or toolchain to tell me about it.

There are a number of ways to include code from external files with
Pandoc, but I will shamelessly refer to my own filter called
[pandoc-include-code][], which I use extensively. To include a
source code file, write an empty fenced code block and use the `include`
attribute to specify the path to the external file:

```` {.markdown}
``` {.javascript include=my-program.js}
```
````

Now, suppose you have a Haskell program in a file `Sample.hs`.

``` {.haskell include=_posts/pandoc-beamer-examples/Sample.hs}
```

The issue is you want to include just the `Animal` data type and the
`isAfraidOf` definition, not the top module declaration and the
`result` definition. By wrapping the content in two special comments,
`start snippet <name>` and `end snippet <name>`, you create a named
snippet:

``` {.haskell include=_posts/pandoc-beamer-examples/Sample2.hs}
```

In the Markdown source, refer to the snippet's name when including:

```` markdown
``` {.haskell include=Sample2.hs snippet=animals}
```
````

The included code will be only that in your snippet:

``` {.haskell include=_posts/pandoc-beamer-examples/Sample2.hs snippet=animals}
```

You can still compile the code, load it in the REPL, and write tests
for it, while including interesting parts into your slides. You are
not depending on specific line number ranges, which of course becomes
a nightmare once you edit your source code.

The last feature of [pandoc-include-code][] I want to demonstrate is
the `dedent` attribute. Let's say we have a Javascript file with a
class method that you're interested in:

``` {.javascript include=_posts/pandoc-beamer-examples/sample1.js}
```

When including snippets of indented source code, you might want to
"dedent", i.e. remove extra leading whitespace. This is easily
accomplished with the `dedent` attribute, specifying how many
whitespace characters you want removed:

```` markdown
``` {.javascript include=sample1.js snippet=bar dedent=2}
```
````

The included code will be "dedented" to the first column:

``` {.javascript include=_posts/pandoc-beamer-examples/sample1.js snippet=bar dedent=2}
```

## Generating Diagrams

Often I want a couple of diagrams in a presentation, to illustrate
some design or flow in a program. I enjoy generating diagrams from
plain text sources, instead of drawing by hand or using special
drawing software with binary formats. Both [Graphviz][] and
[PlantUML][] are powerful tools that are relatively easy to integrate
with the presentation build in a Makefile.

Let's say I want to generate a state diagram. The following Graphviz
source code generates a simple yet beautiful diagram:

``` {.graphviz include=_posts/pandoc-beamer-examples/diagrams/door.dot}
```

Generate a PNG file using the `dot` command:

``` shell
dot -Tpng -o door.png door.dot
```

The generated PNG image looks like this:

![The state diagram generated by Graphviz.](/generated/pandoc-beamer-examples/door.png)

To automate this process with Make, you can find all `.dot` files,
transform those paths into a list of target paths, and have Make run
the `dot` command to generate all targets.

``` makefile
DIAGRAM_SRCS=$(shell find src -name '*.dot')
DIAGRAMS=$(DIAGRAM_SRCS:src/%.dot=target/%.png)

.PHONY: all
all: $(DIAGRAMS)

target/%.png: src/%.dot
	mkdir -p target
	dot -Tpng $< -o $@
```

## A Full Template

[^1]: I once needed to convert a technical manual from ODF to
    reStructuredText. A single Pandoc command later, and I had the
    sources for a proper Sphinx build.

[Listings]: http://texdoc.net/texmf-dist/doc/latex/listings/listings.pdf
[Pandoc]: https://pandoc.org
[pandoc-include-code]: https://github.com/owickstrom/pandoc-include-code
[Graphviz]: http://graphviz.org
[PlantUML]: http://plantuml.com
