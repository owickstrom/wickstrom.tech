---
title:  "Statically Typed Functional Programming with Python 3.12"
date: May 18, 2024
author: Oskar Wickström
---

Lately I've been messing around with Python 3.12, discovering new features around typing and pattern matching.
Combined with dataclasses, they provide support for a style of programming that I've employed in Kotlin and Typescript at work.
That style is in turn is based on what I'd do in OCaml or Haskell, like modelling data with algebraic data types.
However, the more advanced concepts from Haskell --- and OCaml too, I guess --- don't transfer that well to such mainstream languages.

What I'm describing in this post is a useful trade-off that I find comfortable to use in Python, especially with the new features that I'll describe. 
Much of this works nicely in Kotlin and Typescript, too, with minor adaptions.
The principles I try to use are:

Declarative rather than imperative operations on data
: Transform or fold data using `map`, `reduce`, or for-comprehensions instead of for-loop

Functions with destructuring pattern-matching to dispatch based on data
: Use `when` statements rather than `if instanceof(...)` or inheritance and dynamic dispatch

Programs structured mainly around data and functions
: Programs are trees of function invocations on data, rather than class hierarchies, dependency injection, and exceptions

Effects pushed to the outer layers of the program (hexagonal architecture)
: Within reasonable bounds, functions in the guts of programs are _pure_ and return data, whereas the outer layers
interpret that data and manage effects (IO, non-determinism, etc)

This list is not exhaustive, but I'm trying to keep this focused.
Also, I'm intentionally not taking this in the direction of Haskell, with typeclass hierarchies, higher-kinded types, and so on.
I don't believe cramming such constructs in would benefit Python programs in practice.

The examples are all type-annotated and checked with Pyright. You could do all of this without
static type-checking, as far as I know.

Finally, note that I consider myself a Python rookie, as I mostly use it for small tools and scripts.
The largest program I've written in Python is [Quickstrom](https://github.com/quickstrom/quickstrom).
This post is meant to inspire and trigger new ideas, not to tell anyone how to write Python code.

All right, let's get started and see what's possible!

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

## Pattern Matching

Let's start with a classic example from the functional programming world: an evaluator for a simple expression-based language.
It only supports a few operations in order to keep it simple.
First, I model the different types of expressions there are using dataclasses and a union type:


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
```

The syntax for type aliases and the union type operator (`|`) are both new additions.
You could create type aliases before using regular top-level bindings, but mutually recursive types required some types to be enclosed in strings.
Otherwise, Python would complain that the second type (e.g. `BinOp` in the code above) wasn't defined.
It's a bit cleaner now.

Note that I use existing primitive types from Python (`int`, `bool`, and `str`), combined with dataclasses for complex expressions.
The `str` is interpreted as a reference to a name bound in the lexical scope, not as a string literal, as we'll see in the following snippet.

The evaluator tracks bindings in the `Env`.
Evaluating an expression results in a value that is either an integer or a boolean.

```python
type Env = dict[str, Value]

type Value = int | bool
```

Now, let's look at the `eval` function.
Here I pattern-match on the expression, which is a union.
For literals, I just return the value:

```python
def eval(env: Env, expr: Expr) -> Value:
    match expr:
        case int() | bool():
            return expr
        ...
```

References are looked up in the environment:

```python
case str():
    return env[expr]
```

Let-bindings create a new environment with the new binding:

```python
case Let(name, value, body):
    new_env = env | {name: eval(env, value)}
    return eval(new_env, body)
```

Finally, the `BinOp` and `If` branches pattern-match on the evaluated nested expressions to make sure they're of the correct types:

```python
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

Nice!
But this is far from a robust evaluator. 
If we run it with a deep enough expression, we'd get a `RecursionError` saying that the maximum recursion depth was exceeded.
This is a commonly occurring problem when writing recursive functions in Python.[^1]
The `eval` function could be rewritten with an explicit stack for operations and operands, but it's a bit fiddly.

In some cases, you can restructure a recursive function as tail-recursive, and then manually convert it to a loop.
Perhaps you could automatically optimize tail-calls, or use a [trampoline](https://pypi.org/project/trampoline/).
Some solutions avoid stack overflows, at the expense of increased heap memory usage.
In simpler cases, combinators like `map` and `reduce` might suffice, instead of explicit recursion.

Either way, recursive functions and stack overflow is something to watch out for.

## Generics

Since Python 3.12, it's also much nicer to work with generic types. 
Previously, you had to define type variables before using them in type signatures.
This felt very awkward to me.

Let's look at an example that models a rose tree data type.
To spice it up a little, I'm including a `map` function for both types of the tree nodes.
This is equivalent to [`fmap`](https://hackage.haskell.org/package/base-4.20.0.0/docs/Data-Functor.html#v:fmap) in Haskell, but without the typeclass and higher-order types.

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

Let's print these trees using pattern matching.
Here's a function that's written as a loop, maintaining a list of remaining sub-trees to print:

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

It could be even simpler using plain recursion, but then we could run into stack depth issues again.
Anyway, let's try it out:

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

As you can see from the `repr` being printed, all the values are mapped to strings.

## Protocols

As a last example, I'd like to show how you can do basic structural subtyping using _protocols_.
This is useful in cases where you don't want to define all variants of a union in a single place.
For instance, you might have many types of events that can be emitted in an application.
Centrally defining each type of event adds unwanted coupling.
Breaking apart a base class for events and the code that later on consumes the events decreases cohesion.
In such cases, a protocol might be a better option.

We'll need a new import:

```python
from datetime import datetime
```

Now, consider the following events in module A:

```python
@dataclass
class Increment[Time]:
    id: str
    time: Time

    def description(self: Self) -> str:
        return "Incremented counter"


@dataclass
class Reset[Time]:
    id: str
    time: Time

    def description(self: Self) -> str:
        return "Reset counter"
```

I made them generic just to showcase the combination of protocols and generics.
The `Time` type parameter isn't instantiated in any other way than `datetime` in this example.

In another module B --- that doesn't depend on A, and isn't depended upon by A --- the protocol is defined, 
along with the `log_event` function:

```python
class Event[Time](Protocol):
    id: str
    time: Time

    def description(self: Self) -> str: ...

def log_event(event: Event[datetime]):
    print(f"Got {event.id} at {event.time}: {event.description()}")
```

`Increment` and `Decrement` both implement the `Event` protocol by virtue of being structurally compatible.
They can both be passed to `log_event`:

```python
log_event(
    Increment("foo", datetime.now()),
)
```

If I annotate the `Event` protocol with `@runtime_checkable`, I can check
it with `isinstance` and use it in match cases:


```python
@runtime_checkable
class Event[Time](Protocol):
  ...

def log(x: Any):
    match x:
        case Event() if isinstance(datetime, x.time):
            log_event(x)
        case _:
            print(x)
```

Pretty neat!

That's all I have for now.
Maybe more Python hacking and blog posts will pop up if there's interest.
I'm positive to the evolution of Python and functional programming, as it's something I use quite regularly.

[^1]: See [On Recursion, Continuations and Trampolines](https://eli.thegreenplace.net/2017/on-recursion-continuations-and-trampolines/) for more in-depth explanations of various solutions to recursive functions and the stack.
