---
layout: post
title: Automating the Build of your Technical Presentation
slug: automating-the-build-of-your-technical-presentation
date: 2017-09-24
author: Oskar Wickström
category: programming
tags: ["automation", "presentation", "pandoc", "make", "latex"]
excerpt: |
    Writing technical presentations that include code samples and diagrams
    can a tedious task of copying, pasting, and fixing errors. This article
    demonstrates a setup that automates the process and lets you focus on
    your creative process.
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
diagrams, all at the same time. Having to repeat the time-consuming
and error-prone process of updating code samples in slides, each time
my original source code changes, breaks my creative flow completely. I
also want to have my source code _compiled and executable_, so that I
can be confident it is correct.

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
Pandoc Markdown, Beamer, Graphviz and Make. I have also created a
template, based on my setup, that you can use if you like this
approach.

## Writing Slides with Pandoc Markdown

One of my favorite tools in technical writing is [Pandoc][]. I use it
for documentation, talks, Markdown preview, this article[^1],
and for converting existing documents to more desirable formats[^2].

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

Voilà! You have a PDF, such as [this
one](/generated/pandoc-beamer-examples/first.pdf).

You might want to customize some of the Beamer styling, which is done
by including a `.tex` file using the `-H` command line parameter of
Pandoc. The full template described below uses this technique to
change the styling.

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

``` {.dot include=_posts/pandoc-beamer-examples/diagrams/door.dot}
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
	mkdir -p $(shell dirname $@)
	dot -Tpng $< -o $@
```

A similar setup can be used with [PlantUML][], although you might want the
JAR file to download automatically:

``` makefile
PLANTUML=deps/plantuml.jar

UML_SRCS=$(shell find src -name '*.uml.txt)
UMLS=$(UML_SRCS:src/%.uml.txt=target/%.png)

.PHONY: all
all: $(UMLS)

target/%.png: src/%.uml.txt $(PLANTUML)
	mkdir -p $(shell dirname $@)
	cat $< | java -jar $(PLANTUML) -tpng -pipe > $@

$(PLANTUML):
	mkdir -p $(shell dirname $@)
	wget http://sourceforge.net/projects/plantuml/files/plantuml.jar/download -O $@
```

I have used PlantUML in this blog, just as described above, to
generate diagrams for posts. See the post [Hyper: Elegant Weapons for
a More Civilized
Page](/programming/2017/01/06/hyper-elegant-weapons-for-a-more-civilized-page.html)
for an example.

## Wrapping Up

Based on the techniques described in this post, I have created a
template that you can use for your own presentations. It is
[published at GitHub][template]. I hope this will be useful to
someone, and that it can be a good complement to this article.

What I really like about the tools and techniques demonstrated in this
article is that they are not tied to presentations. I use the same
tools for writing documentation, and for writing this blog! Pandoc is
an amazing piece of software, and I have just scratched the surface of
what it can do. For instance, if you do not want PDF output for your
talk, there's a number of Javascript-based formats for slideshows
available.

Now go on and write some cool tech talks!

## Footnotes

[^1]: The [source
code](https://raw.githubusercontent.com/owickstrom/func-da-world/master/src/_posts/2017-09-24-pandoc-beamer-graphviz.md)
for this article uses some of the techniques it describes. Warning,
there's nested fenced code blocks; not something for the weak-hearted.
[^2]: I once needed to convert a technical manual from ODF to
    reStructuredText. A single Pandoc command later, and I had the
    sources for a proper Sphinx build.

[Listings]: http://texdoc.net/texmf-dist/doc/latex/listings/listings.pdf
[Pandoc]: https://pandoc.org
[pandoc-include-code]: https://github.com/owickstrom/pandoc-include-code
[Graphviz]: http://graphviz.org
[PlantUML]: http://plantuml.com
[template]: https://github.com/owickstrom/automating-the-build-of-your-technical-presentation-template
