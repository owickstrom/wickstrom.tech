---
title: "Error Reporting in Linear Temporal Logic" 
date: "October 24, 2025"
author: "Oskar Wickström"
---

* Intro
    * Logic errors, unsat
    * Linear temporal logic, QuickLTL, and Quickstrom
    * Why error reporting matters
    * Error Reporting Logic reference and summary
* Picostrom and error reporting
    * QuickLTL recap
    * Introduce picostrom-rs
    * Motivating examples
* Plain text only?
    * Maybe show some huge error with lots of parts, hard to parse
    * Show the diagram example
    * Other wild ideas
* Things left out
    * Implication
    * `next (always X)` only shows inner error when failed


Let's say we have a failing property like the following:

$$\text{next}_d(\text{next}_d(\text{always}_10(\text{B} < \text{C})))$$

The textual error might be:

> **Definitely false:** as of state 3, it must always be the case that B is greater
> than C, but in state 6, B (13) is not greater than C (15)

But we could also draw a diagram, using information from the collected states:

<pre>
            <span class="blue">□ As of state 3, it must always be the case</span>
            <span class="blue">╎ that B is greater than C.</span>
            <span class="blue">╎</span>
            <span class="blue">╎</span>               <span class="red">✗ In state 6, B (13) is not</span>
            <span class="blue">╎</span>               <span class="red">╎ greater than C (15).</span>
  Value     <span class="blue">╎</span>               <span class="red">╎</span> 
            <span class="blue">╎</span>               <span class="red">╎</span> 
    ║       <span class="blue">╎</span>               <span class="red">╎</span>                
15  ║       <span class="blue">╎</span>               ┌──────────────── C
    ║       <span class="blue">╎</span>               │                  
    ║       <span class="blue">╎</span>             ┌─│──────────────── B
    ║─────────────────────┘ │
    ║       <span class="blue">╎</span>               │
10  ║       <span class="blue">╎</span>               │
    ║       <span class="blue">╎</span>               │ 
    ║       ┌───────────────┘
    ║───────┘               <span class="red">╎</span>
    ║       <span class="blue">╎</span>               <span class="red">╎</span>
 5  ║       <span class="blue">╎</span>               <span class="red">╎</span>
    ║       <span class="blue">╎</span>               <span class="red">╎</span> 
    ║       <span class="blue">╎</span>               <span class="red">╎</span>
    ║       <span class="blue">╎</span>               <span class="red">╎</span>
    ║       <span class="blue">╎</span>               <span class="red">╎</span>
 0  ╚══════════════════════════════════════════ State
    1   2   3   4   5   6   6   7   8   9   10
</pre>
