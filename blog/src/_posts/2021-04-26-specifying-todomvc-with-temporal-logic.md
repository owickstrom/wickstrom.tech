---
layout: post
draft: true
title:  "Specifying State Machines with Temporal Logic"
author: Oskar Wickström
date: 2021-04-26
categories: general
tags: []
excerpt: |
    ...
---

## Intro

Quickstrom uses linear temporal logic (LTL) for specification of web
applications. When explaining how it works, I've found that the basics
of LTL are rather intuitive. On the other hand, it's not so easy to
see how to specify stateful systems using LTL. To be clear, I have
learned a lot this past year and I'm not an expert on the subject.

This post focuses on how to use LTL to specify state machines. It's a
quick overview that avoids going into too much detail.

## An Extended LTL

We'll be using an LTL extended with state selectors and actions. The
language is essentially a sketch of the future specification language
for Quickstrom.

## State Transitions

  * States
  * `next`
  * Describing transitions
  * `always`
  * Safety properties

## Until

## Eventually

  * Liveness properties

## Formulae

  * Atomic propositions
  * Implicit lifting

## State-dependence

  * State selectors
  * Objects

## Actions

  * Preconditions

## Events 

  * They are actions
  * Postconditions

## What happened?

  * The `happened` binding is a list of actions or events that happened between the last and the current state

## Summary
  * ...

