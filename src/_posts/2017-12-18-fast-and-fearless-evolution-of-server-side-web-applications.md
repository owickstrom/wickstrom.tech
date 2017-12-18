---
layout: post
title: Fast and Fearless Evolution of Server-Side Web Applications
author: Oskar Wickström
categories: programming
tags: ["haskell", "tools", "web", "type systems", "yesod", "scotty", "airship"]
excerpt: |
  This is a blog post based on parts of my talk "Fast and Fearless Evolution of
  Server-Side Web Applications," about type safety and mature web technology
  that enables evolving software with greater confidence.
---

*This is a blog post based on parts of my talk "Fast and Fearless Evolution of
Server-Side Web Applications." I gave this talk at the [f(by) conference in
Minsk, Belarus, December 17th 2017](https://fby.by/). The original talk has
examples of three Haskell web frameworks, with many code examples, and some
recommendations around client-side technologies to use. This post, however,
excludes the code examples, and focuses on the motivation. If you are
interested, you can find the [slides and code examples on
GitHub][slides-code].*

## Evolving Software

There are many reasons to evolve our software over time. We get new feature
requests, fix bugs, and refactor code. Moreover, there are external factors
forcing us to change the software, like deprecation of libraries and external
services. Finally, we might pick up a new technology for recruiting purposes,
attracting people to join our company.

Evolving software, we deal with risk, e.g. delays, errors, and burnout. These
risks make it scary to evolve software *freely*. Let's say we have previously
misunderstood an aspect of the business process, and that we want to modify our
according to our improved understanding. The fear of introducing errors while
making the change, not delivering it on time, or taking on a huge workload that
hurts the team, often leads to bad design decisions. Instead of making the
change we want, scared of all the things that could go wrong, we hack our way
around the risk.

Tools can reduce the fear. By giving us stronger correctness guarantees, and
robust run-time behavior, these tools increase our confidence in making
fundamental changes to our software. Worth emphasizing is that we *reduce*
fear, and give *stronger* guarantees. We do not eliminate the fear completely,
and our programs are *not* necessarily correct just because they compile. You
can still write an incorrect program. We can, however, eliminate large classes
of bugs by using these tools.

## Server-Side Web Applications

Many programmers work with the web somehow. If not directly, writing web
applications, then indirectly in systems that eventually interact the web. If
you have worked in front-end web development recently, you probably know how
single-page web applications (SPA) are in vogue. They work more like
traditional desktop web applications, or what is known as *fat clients*.

SPA frameworks tend to reinvent parts of the web browser, e.g. custom routers,
link click interception, and form replacements using JSON and XHR.
Furthermore, they require Javascript to run. Now, you might be thinking
"Hey, that's not right, we have solved that with universal web apps!" Sure,
you have *initial rendering* based on the same data access and view code,
but can your universal web application framework *transparently* run a
single application both client-side and server-side? I think not, but I am
eagerly waiting to be proven wrong.

### Newer Does Not Imply Better

I think web development is suffering from a belief that we need to constantly
use the newer thing. In the context of web applications, it might be the latest
SPA framework. I urge you to not dismiss server-side web applications on the
basis of them being old. In this case, old is good. Maturity is good. Instead,
use server-side as the default, and do *progressive enhancement*, enabling
client-side capabilities as a *bonus*, and leaning on the maturity of the web
browser, HTTP, and regular links and forms.

### Your Complexity Budget

Related to progressive enhancement is the observation that all of your code is
not equally valuable. In addition to your core domain functionality, the thing
you are actually doing business based on, you probably have loads of supporting
code; settings pages and forms, signup and login, documentation, user forums,
and back office functions.

These things also need to exist, but they are not the reason why you are in
business. Spending your *complexity budget* in these areas of your system
is wasteful. Instead, use simple low-risk techniques for those features,
reducing fear of change in the future. In the context of web applications, I
recommend using single-page applications where a server-side application would
hurt your business, not as a default for your entire system.

## Static Typing for Server-Side Web

Using an expressive static type system, we can make parts of our program
*correct by construction*. In the same spirit of applying single-page
application technology where valuable, complex static typing techniques should
be applied with care.  In Haskell, for instance, this means weighing the
benefits and drawbacks in using certain language extensions, or using complex
type-level techniques. Your team needs to agree on a good trade-off for your
project, finding where more advanced techniques provide value and not only
serve as exciting type system puzzles to solve.

The fear of changing code, even in large code bases, is significantly reduced
when using a powerful static type system, especially in a pure language like
Haskell. You can modify the data types and functions of your core domain,
follow the errors, and have the compiler guide you in making the appropriate
changes. Again, you can introduce logic errors, but the risk of programming
errors is very low, in my experience. When building a server-side web
application, you have a single code base, not split by an HTTP API, checked by a
single type-checker. This makes broad changes to the code base safer to do,
especially if you have type-safe HTML templates and routing.

There are many languages and framework offering various levels of type safety.
Look for the patterns and guarantees you get, like type-safe URLs and routing,
templates, and safety features around injection and cross-site scripting, to
name a few. The key idea, underlying the amazing refactoring story in Haskell,
is this:

**Less power is more power.**

Having side effects tracked in types is a huge benefit when changing code. The
safety and composability you get from a pure functional language is, to me,
truly astounding. I have mentioned Haskell, and the original talk has examples
of using various Haskell web frameworks, but these ideas are not limited to
Haskell.

## Key Takeaways

The three central ideas I want to emphasize, and which I think should be
discussed and actively thought through in web application development, are:

<div class="takeaway">
![](/assets/brain.png){width=50%}

### Evolve software fearlessly using better tools for modeling and communication.

Your program is a communication between you, your colleagues, and the computer.
Use tools that support that communication. Use tools that help you evolve your
ideas continually.
</div>

<div class="takeaway">
![](/assets/tokens.jpeg){width=75%}

### Spend your complexity budget carefully.

Large parts of your web application has lower value than the core business
parts. In those areas, reach for simple tools with less risk. When needed, use
more advanced tools, where you get a return on the complexity investment.
</div>

<div class="takeaway">
![](/assets/mwa-logo.png){width=20%}

### Explore the wonderful world of functional and statically typed server-side web.

Combine the powers of statically typed functional programming and
server-side web development, and you have very good toolbox for evolving web
systems.
</div>

Again, the slides and code examples for the talk are available [on
GitHub][slides-code], and the talk will be uploaded to YouTube in a week or
two. By the way, f(by) was a really well-organized and welcoming conference,
and I met a lot of interesting and nice people!

[slides-code]: github.com/owickstrom/fast-and-fearless-evolution-of-server-side-webapps](https://github.com/owickstrom/fast-and-fearless-evolution-of-server-side-webapps
