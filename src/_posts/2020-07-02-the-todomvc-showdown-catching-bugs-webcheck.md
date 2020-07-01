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
behavior as a finite-state machine,, using a logic formula heavily inspired by
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

Early on, checking the mainstream framework examples, I found that both
Angular and Mithril were rejected by my specification, and [I posted as an
issue](https://github.com/tastejs/todomvc/issues/2116) in the TodoMVC issue
tracker. Invigorated by the initial success, I decided to check the remaining
examples and gradually improve my specification. In this post I'll share the
results I've got so far.

## Test Results

Below are the test results from running WebCheck and my TodoMVC specification
on the examples listed on the TodoMVC website. I'll use short descriptions of
the problems (some of which are recurring), and explain more in detail
further down.

<table style="width: 100%;">
    <thead>
        <tr>
            <th style="width: 1%;"></th>
            <th>Example</th>
            <th>Problems</th>
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
                    <li>Needs custom <code>readyWhen</code> condition</li>
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
                    <li>Inconclusive, can't clear state between test runs</li>
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
            <td>404 Not Found</td>
        </tr>
        <tr class="divider"><td colspan="3">Non-framework implementations</td></tr>
        <tr>
            <td>❌</td>
            <td>VanillaJS</td>
            <td>Adds pending item on other iteraction</td>
        </tr>
        <tr>
            <td>❌</td>
            <td>VanillaJS ES6</td>
            <td>Adds pending item on other iteraction, minor: <code>.todo-count strong</code> is missing</td>
        </tr>
        <tr>
            <td>✓</td>
            <td>jQuery</td>
            <td></td>
        </tr>
    </tbody>
</table>

1. Filters not implemented
1. Race condition in initialization: focus input and press Return before 
   event listeners are attached, results in `index.html?text=` 
1. Inconsistent first render: Shows an empty list items and "0 left" for a short moment, then renders the valid initial state.
1. Adds pending item on other iteraction (toggle all, change filter)

1. Needs custom `readyWhen` condition: Needs a modified spec that awaits framework-specific class, e.g. `".ng-scope"`
1. No input field

_If you have any comments, please reply to [this Twitter
thread]()._
