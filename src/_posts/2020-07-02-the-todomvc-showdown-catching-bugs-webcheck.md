---
layout: post
title: "The TodoMVC Showdown: Catching Bugs using WebCheck"
author: Oskar Wickström
date: 2020-07-01
categories: programming
tags: ["testing", "specification", "formal-methods", "browser"]
excerpt: |
  WebCheck is a browser testing framework
  combining ideas from property-based testing, TLA+ and linear temporal
  logic, and functional programming. In this post I'll share my results
  of testing TodoMVC implementations using WebCheck.
---

During the last three months I've used my spare time to build
[WebCheck](https://webcheck.tools). It's a browser testing framework
combining ideas from:

- property-based testing
- TLA+ and linear temporal logic
- functional programming

In WebCheck, you don't write test cases for your web application. Instead,
you write a _specification_. The specification describes the intended
behavior as a finite-state machine, using a logic formula heavily inspired by
the _temporal logic of actions_ from TLA+.

As opposed to property-based testing with state machine models, you don't
have to implement a full model of your system. You can leave out details
and specify only the most important aspects of your system.

Since the start of this project, I've used the [TodoMVC](http://todomvc.com/)
examples as a benchmark of WebCheck, and developed a general specification
for TodoMVC applications. The TodoMVC contribution docs has [a high-level
feature
specification](https://github.com/tastejs/todomvc/blob/master/app-spec.md),
and they do have a Cypress test suite, but I was curious if I could find
anything new using WebCheck.

Early on, checking the mainstream framework examples, I found that both the
Angular and Mithril examples were rejected by my specification, and [I
submitted an issue](https://github.com/tastejs/todomvc/issues/2116) in the
TodoMVC issue tracker. Invigorated by the initial success, I decided to check
the remaining examples and gradually improve my specification. In this post
I'll share the results I've got so far.

## Test Results

Below are the test results from running WebCheck and my TodoMVC specification
on the examples listed on the TodoMVC website. I'll use short descriptions of
the problems (some of which are recurring), and explain more in detail
further down.

<table style="width: 100%;" class="todomvc-results">
    <thead>
        <tr>
            <th style="width: 1%;"></th>
            <th>Example</th>
            <th>Problems/Notes</th>
        </tr>
    </thead>
    <tbody>
        <tr class="divider"><td colspan="3">Pure JavaScript</td></tr>
        <tr>
            <td>✓</td>
            <td>Backbone.js</td>
            <td></td>
        </tr>
        <tr>
            <td>❌</td>
            <td>AngularJS</td>
            <td><ul><li>Clears input field on filter change</li></ul></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Ember.js</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Dojo</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Knockback</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>CanJS</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Polymer</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>React</td>
            <td></td>
        </tr>
        <tr>
            <td>❌</td>
            <td>Mithril</td>
            <td><ul><li>Clears input field on filter change</li></ul></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Vue</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>MarionetteJS</td>
            <td></td>
        </tr>
        <tr class="divider"><td colspan="3">Compiled to JavaScript</td></tr>
        <tr>
            <td>✓</td>
            <td>Kotlin + React</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Spine</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Dart</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>GWT</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Closure</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Elm</td>
            <td></td>
        </tr>
        <tr>
            <td>❌</td>
            <td>AngularDart</td>
            <td>
                <ul>
                    <li>Race condition on initialization</li>
                    <li>Filters not implemented</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td>✓</td>
            <td>TypeScript + Backbone.js</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>TypeScript + AngularJS</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>TypeScript + React</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Reagent</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Scala.js + React</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Scala.js + Binding.scala</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>js_of_ocaml</td>
            <td></td>
        </tr>
        <tr>
            <td>&ndash;</td>
            <td>Humble + GopherJS</td>
            <td><ul><li>404 Not Found</li></ul></td>
        </tr>
        <tr class="divider"><td colspan="3">Under evaluation by TodoMVC</td></tr>
        <tr>
            <td>✓</td>
            <td>Backbone.js + RequireJS</td>
            <td></td>
        </tr>
        <tr>
            <td>❌</td>
            <td>KnockoutJS + RequireJS</td>
            <td>
                <ul>
                    <li>Inconsistent first render</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td>✓</td>
            <td>AngularJS + RequireJS</td>
            <td>
                <ul>
                    <li>Needs a custom <code>readyWhen</code> condition</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td>✓</td>
            <td>CanJS + RequireJS</td>
            <td></td>
        </tr>
        <tr>
            <td>❌</td>
            <td>Lavaca + RequireJS</td>
            <td>
                <ul>
                    <li>Clears input field on filter change</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td>❌</td>
            <td>cujoJS</td>
            <td>
                <ul>
                    <li>Race condition on initialization</li>
                    <li>Filters not implemented</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Sammy.js</td>
            <td></td>
        </tr>
        <tr>
            <td>&ndash;</td>
            <td>soma.js</td>
            <td>
                <ul>
                    <li>404 Not Found</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td>❌</td>
            <td>DUEL</td>
            <td>
                <ul>
                    <li>Clears input field on filter change</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Kendo UI</td>
            <td></td>
        </tr>
        <tr>
            <td>❌</td>
            <td>Dijon</td>
            <td>
                <ul>
                    <li>Filters not implemented</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Enyo + Backbone.js</td>
            <td></td>
        </tr>
        <tr>
            <td>❌</td>
            <td>SAPUI5</td>
            <td>
                <ul>
                    <li>No input field</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Exoskeleton</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Ractive.js</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>React + Alt</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>React + Backbone.js</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Aurelia</td>
            <td></td>
        </tr>
        <tr>
            <td>❌</td>
            <td>Angular 2.0</td>
            <td>
                <ul>
                    <li>Filters not implemented</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Riot</td>
            <td></td>
        </tr>
        <tr>
            <td>✓</td>
            <td>JSBlocks</td>
            <td></td>
        </tr>
        <tr class="divider"><td colspan="3">Real-time</td></tr>
        <tr>
            <td>&ndash;</td>
            <td>SocketStream</td>
            <td>
                <ul>
                    <li>State cannot be cleared</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td>✓</td>
            <td>Firebase + AngularJS</td>
            <td></td>
        </tr>
        <tr class="divider"><td colspan="3">Node.js</td></tr>
        <tr>
            <td>&ndash;</td>
            <td>Express + gcloud-node</td>
            <td>
                <ul>
                    <li>404 Not Found</li>
                </ul>
            </td>
        </tr>
        <tr class="divider"><td colspan="3">Non-framework implementations</td></tr>
        <tr>
            <td>❌</td>
            <td>VanillaJS</td>
            <td>
                <ul>
                    <li>Adds pending item on other iteraction</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td>❌</td>
            <td>VanillaJS ES6</td>
            <td>
                <ul>
                    <li>Adds pending item on other iteraction</li>
                    <li><code>.todo-count strong</code> is missing</li>
                </ul>
            </td>
        </tr>
        <tr>
            <td>✓</td>
            <td>jQuery</td>
            <td></td>
        </tr>
    </tbody>
    <tfoot>
        <tr class="todomvc-legend">
            <td colspan="3">✓ Passed, ❌ Failed, &ndash; Not testable</td>
        </tr>
    </tfoot>
</table>


Filters not implemented

: There's no way of switching between "All", "Active", and "Completed" items.
This is specified in the TodoMVC documentation under
[Routing](https://github.com/tastejs/todomvc/blob/master/app-spec.md#routing).

Race condition during initialization

: The event listeners are attached some time after the `.new-todo` form is
rendered. Although unlikely, if you're quick enough you can focus the input,
press <kbd>Return</kbd>, and post the form. This will navigate the user agent
to the same page but with a query paremeter, e.g. `index.html?text=`. In
TodoMVC it's not the end of the world, but I suspect there are real systems
where you do not want this to happen.

Inconsistent first render
: The application briefly shows an inconsistent view, then renders the valid initial state. _KnockoutJS + RequireJS_ shows an empty list items and "0 left" in the bottom, even though the footer [should be hidden when there are no items](https://github.com/tastejs/todomvc/blob/master/app-spec.md#no-todos).

Needs a custom `readyWhen` condition

: The specification awaits an element matching `.todoapp` (or `#todoapp` for
the old markup) in the DOM before taking any action. In this case, the
implementation needs a modified specification that instead awaits a
framework-specific class, e.g. `.ng-scope`. I wouldn't classify this as a
bug, just an inconvenience in testing the implementation using WebCheck.

No input field

: There's no input field to enter TODO items in. I'd argue this defeats the
purpose of a TODO list application, and it's [indeed specified in the offical
documentation](https://github.com/tastejs/todomvc/blob/master/app-spec.md#new-todo).

Adds pending item on other iteraction

: When there's a pending item in the input field, and another action is taken
(toggle all, change filter, etc), the pending item is submitted
automatically without a <kbd>Return</kbd> key press.

`.todo-count strong` element is missing

: An element matching the selector `.todo-count strong` must be present in
the DOM when there are items, showing the number of active items, as
described [in the TodoMVC
documentation](https://github.com/tastejs/todomvc/blob/master/app-spec.md#counter).

State cannot be cleared

: This is not a bug, but an issue where the test implementation makes it hard
to perform repeated isolated testing. State cannot (to my knowledge) be
cleared between tests, and so isolation is broken. This points to a key
requirement currently placed by WebCheck: the SUT is must be stateless, with
respect to a new private browser window. In future versions of WebCheck,
hooks should be added where the tester can clear the system state before
tests are run.

### Unspecified parts

The specification doesn't cover all of the TodoMVC behavior yet. Most
notably, it leaves out the [editing
mode](https://github.com/tastejs/todomvc/blob/master/app-spec.md#editing)
entirely. I might add it later, but I think I've found enough to motivate
using WebCheck on TodoMVC applications. Further, this is likely how WebCheck
would be used in other projects. You specify some things and you leave out
others.

## How does it work?

If you've read this far, I bet you're interested in the specification. To
keep this blog post brief, I've uploaded a poorly documented version of the
specification [as a gist](
https://gist.github.com/owickstrom/1a0698ef6a47df07dfc1fe59eda12983). Note
that I've removed support for the old markup (using IDs instead of classes)
to keep it as simple as possible.

The astute reader might have noticed that it looks like PureScript. And it
pretty much is PureScript, with some WebCheck-specific additions for temporal
modalities and DOM queries.

As for how WebCheck itself works, that's for a future post.

## The Future is Bright

I'm happy with how effective WebCheck has been so far, after only a few
months of spare-time prototyping. Hopefully, I'll have something more
polished that I can make available soon. When that time comes, maybe WebCheck
can be part of the TodoMVC project's testing. I'd be very happy about that,
at least.

If you're interested in WebCheck, please [sign up for the
newsletter](https://buttondown.email/webcheck). I'll post regular project
updates, and definitely no spam. You can also follow me [on
Twitter](https://twitter.com/owickstrom).

## Comments

_If you have any comments, please reply to [this Twitter thread]()._