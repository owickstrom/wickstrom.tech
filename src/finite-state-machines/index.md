---
layout: category
title: Finite-State Machines
category: finite-state-machines
---

Stateful programs often become complex beasts when they grow. Program
state incohesively spread across a bunch of variables, spuriously
guarded by even more variables, is what I refer to as *implicit
state*. When working with such code, we have to reconstruct a model
mentally, identifying possible states and transitions between them, to
modify it with any certainty. That process is tedious and error-prone,
and I insist we should have our tools do the heavy lifting instead.

This is a series of post based on the subject of using expressive type
systems and functional programming to encode finite-state machines.
