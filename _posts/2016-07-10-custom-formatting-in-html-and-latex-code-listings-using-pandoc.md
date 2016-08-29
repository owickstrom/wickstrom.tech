---
layout: post
title:  "Custom Formatting in HTML and LaTeX Code Listings using Pandoc"
date:   2016-07-10 17:00 +0200
author: Oskar Wickström
categories: programming
tags: ["html", "latex", "documentation"]
excerpt: "When working on the Oden User Guide I developed a way of including pre-formatted code listings in HTML and LaTeX."
---

I have worked intensively on the [Oden User Guide](https://oden-lang.org/user-guide/latest/)
lately, primarly on improving content, but also on providing high-quality PDF
and HTML output formats with detailed control over typesetting.

For some code listings and syntax examples I want the typesetting to convey the
meaning of text in the listing -- user input, command output, placeholders,
etc. The User Guide build uses [Pandoc](http://pandoc.org/) to transform
Markdown documents to PDF (through LaTeX) and to HTML. I could not find a good
way to express the custom formatting in a way that worked with both LaTeX
and HTML using standard Pandoc functionality. Therefore, I created a
[filter](http://pandoc.org/scripting.html) to handle my input format, called
[IncludeCode.hs](https://github.com/oden-lang/oden/blob/master/doc/user-guide/src/filters/IncludeCode.hs).
Using this filter, I can write code listings in separate files, using a small
subset of HTML. Here's an example of a shell command listing source file from
the User Guide:

```html
$ <strong>GOPATH=PWD/target/go:$GOPATH go build -o hello hello/main</strong>
$ <strong>./hello</strong>
Hello, world!
```

The `strong` tags in the listing are part of the HTML subset I use. To include
listings in the Pandoc Markdown source I use regular code block syntax and add
the custom `include` and `formatted` attributes:

    ```{include=src/listings/hello-world-go-build-and-run.html formatted=true}
    ```

The output, both in HTML and PDF, looks like this:

<pre><code>$ <strong>GOPATH=PWD/target/go:$GOPATH go build -o hello hello/main</strong>
$ <strong>./hello</strong>
Hello, world!</code></pre>

For listings explaining the syntax of the Oden language I want placeholders to
be typeset in italic text. Where the language supports a sequence of forms I
want to express that using placeholder expressions with subscripts. The
following listing source file explains the *let binding* syntax of Oden.

{% highlight html %}
let <em>identifier<sub>1</sub></em> = <em>expression<sub>1</sub></em>
    <em>identifier<sub>2</sub></em> = <em>expression<sub>2</sub></em>
    ...
    <em>identifier<sub>n</sub></em> = <em>expression<sub>n</sub></em>
in <em>body-expression</em>
{% endhighlight %}

When included in the document, just like in the example before, the output
looks like this:

<pre><code>let <em>identifier<sub>1</sub></em> = <em>expression<sub>1</sub></em>
    <em>identifier<sub>2</sub></em> = <em>expression<sub>2</sub></em>
    ...
    <em>identifier<sub>n</sub></em> = <em>expression<sub>n</sub></em>
in <em>body-expression</em></code></pre>


The filter is very simplistic in that it only supports `em`, `strong`, and
`sub` elements, but it suits the needs of the Oden User Guide. I find extending
Pandoc with filters very powerful, and I hope you find this technique and the
blog post useful. If you are interested in the complete solution, including the
Makefile compiling the filter and running Pandoc, please see
[doc/user-guide](https://github.com/oden-lang/oden/tree/master/doc/user-guide)
in the Oden repository. Also, you might want to have a look at the [PDF version
of the User Guide](https://oden-lang.org/user-guide/latest/user-guide.pdf) to
see the result after LaTeX typesetting.

Long live Pandoc!
