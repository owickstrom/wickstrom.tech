---
title:  "Statically Typed Functional Programming with Python 3.12"
date: May 18, 2024
author: Oskar Wickström
---

* Lately I've messing around with python 3.12
* discovered new features around typing and pattern matching
* combined with dataclasses, provide a style of programming that I enjoy
  - modelling data with algebraic data types (products and unions)
  - declarative operations on data rather than imperative (map instead of for loop)
  - functions with destructuring pattern-matching to dispatch based on data
  - control flow mainly residing in functions, avoiding object-oriented dispatch and inheritance
  - pushing side effects to the outer layers of the program (hexagonal architecture), outside the scope of this post
* I'm intentionally not taking this in the direction of Haskell, typeclass hierarchies, Monads, etc
  - during the last five years I've written a lot of Kotlin, and I've come to enjoy this particular functional style more
  - If there's interest, I might write a post on that specifically
  - This post is focused on the Python features that support that style of programming.
* I consider myself a Python rookie as I mostly use it for small tools and scripts. The largest program I've written in Python is [Quickstrom](https://github.com/quickstrom/quickstrom).
* Let's get started and see what we can do with Python nowadays!

## Preliminaries

First, let's get some boilerplate stuff out of the way.
I'm running Python 3.12.
Some of the things I'll show can be done with earlier versions, but not all.

We'll not be using any external packages, only modules from the standard library:

```python
from typing import *
from dataclasses import dataclass
```

You might not want to use wildcard imports in more serious programs, but it's acceptable for these examples.

## The Evaluator

```python
type Expr = int | bool | str | BinOp | Let | If


@dataclass
class BinOp:
    op: Literal["<"] | Literal[">"]
    lhs: Expr
    rhs: Expr


@dataclass
class Let:
    name: str
    value: Expr
    body: Expr


@dataclass
class If:
    cond: Expr
    t: Expr
    f: Expr


type Env = dict[str, Value]

type Value = int | bool


def eval(env: Env, expr: Expr) -> Value:
    match expr:
        case int() | bool():
            return expr
        case str():
            return env[expr]
        case BinOp(op, lhs, rhs):
            l = eval(env, lhs)
            r = eval(env, rhs)
            match op, l, r:
                case "<", int(), int():
                    return l < r
                case ">", int(), int():
                    return l > r
                case _:
                    raise ValueError(
                        f"Invalid binary operation {op} on {lhs} and {rhs}"
                    )
        case Let(name, value, body):
            new_env = env | {name: eval(env, value)}
            return eval(new_env, body)

        case If(cond, t, f):
            match eval(env, cond):
                case True:
                    return eval(env, t)
                case False:
                    return eval(env, f)
                case c:
                    raise ValueError(f"Expected bool condition, got: {c}")
```


All right, let's try it out:

```python
>>> example = Let("x", 1, If(BinOp("<", "x", 2), 42, 0))
>>> eval({}, example)
42
```

Nice! Again, this is far from a robust interpreter. 
If we run it with a deep enough expression, we'd get a `RecursionError` saying that the maximum recursion depth was exceeded.

## Generics

```python
type RoseTree[T] = Branch[T] | Leaf[T]


@dataclass
class Branch[A]:
    branches: list[RoseTree[A]]

    def map[B](self, f: Callable[[A], B]) -> Branch[B]:
        return Branch([b.map(f) for b in self.branches])


@dataclass
class Leaf[A]:
    value: A

    def map[B](self, f: Callable[[A], B]) -> Leaf[B]:
        return Leaf(f(self.value))
```


We can print these trees with a function that maintains of list of remaining sub-trees to print:

```python
def print_tree[T](tree: RoseTree[T]):
    trees = [(tree, 0)]
    while trees:
        match trees.pop(0):
            case Branch(branches), level:
                print(" " * level * 2 + "*")
                trees = [(branch, level + 1) for branch in branches] + trees
            case Leaf(value), level:
                print(" " * level * 2 + "- " + repr(value))
```

It could be even simpler using recursion, but then we could run into stack depth issues again. Anyway, let's try it out:

```python
example = Branch(
    [
        Leaf(1),
        Leaf(2),
        Branch([Leaf(3), Leaf(4)]),
        Branch([Leaf(5), Leaf(6)]),
        Leaf(7),
    ]
)
>>> print_tree(example.map(str))
*
  - '1'
  - '2'
  *
    - '3'
    - '4'
  *
    - '5'
    - '6'
  - '7'
```

## Procotols

```python
from abc import abstractmethod


class Functor[A](Protocol):
    @abstractmethod
    def map[B](self, f: Callable[[A], B]) -> Functor[B]:
        pass
```
```python
@runtime_checkable
class Named(Protocol):
    name: str


@dataclass
class User:
    name: str


def greet(someone: Any):
    match someone:
        case Named():
            return f"Hello {someone.name}"
        case _:
            return "Hello"
```

