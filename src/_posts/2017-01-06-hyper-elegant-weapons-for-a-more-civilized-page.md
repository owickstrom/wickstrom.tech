---
layout: post
title:  "Hyper: Elegant Weapons for a More Civilized Page"
date:   2017-01-06 06:00 +0200
author: Oskar Wickström
categories: programming
tags: ["hyper", "purescript", "web"]
excerpt: "Since laying Oden aside, I have been getting back into PureScript, and started working on a project called Hyper that I would like to introduce you to."
---

Since laying Oden aside, I have been getting back into PureScript, and started
working on a project called *Hyper* that I would like to introduce you to.

Other than experimenting with extensible records and row typing, and spamming
my followers on Twitter, I have tried to [document the design and
purpose][documentation] of the project so that it will be approachable for
others. Now, however, I feel that a blog post introducing the project without
diving too deep into implementation details would be helpful and interesting.

*If you have any feedback, or like me to write more about Hyper here, please
let me know in the comments at the bottom of this page.*

## Background & Motivation

I have been working with NodeJS for web servers, on and off, for the last 3-4
years. As projects grow, and even at the start of a project, programming errors
creep in around the middleware setup. Many things can go wrong, and they are
often hard to debug. You have no static guarantees that third-party middleware
and your handlers are correctly setup &mdash; that they cover all cases,
execute in the correct order, do not interfere with each other, and handle
errors properly.

After an inspiring talk by Edwin Brady on dependent types for state machines,
I decided to try to improve this situation for web server middleware using
*extensible records* and *row types* in PureScript, to see if I could provide
similiar safety guarantees to those demonstrated in Idris. It turns out that
extensible records work really well for this case!

Another thing circling around in my head, to the point of real concern, is
the amount of energy that gets directed towards pure functional programming in
single-page applications. The ideas are great, and very cool designs emerge,
but I am afraid we completely abandon progressive enhancement and "regular"
server-side rendered web applications. Thus, I wanted to direct some of the
FP love towards good ol' web server software.

I messed around a bit in Haskell with GADTs and phantom types, but PureScript
records really stood out, so I decided to go ahead with PureScript. I think it
deserves a web server library like the one Hyper aims to become. With
alternative PureScript backends emerging, Hyper could also run on Erlang and
C++ servers, not only NodeJS.

## Design

The basic building blocks of Hyper are `Conn` and `Middleware`. They are
heavily inspired by other middleware libraries, like *Plug* in Elixir and
*connect* in NodeJS. The Conn represents the entirety of an HTTP connection, both
the request and response.

```haskell
type Conn req res components =
  { request :: req
  , response :: res
  , components :: components
  }
```

A Conn is a record containing some request `req`, and some response `res`. What
are those values? Well, it depends on the middleware. Usually they are also
records, where middleware specify the fields that they require, or provide, in
both the request and response records. The structure of request and response
records are *open*, and the compiler guarantees that the composition of
middleware is correct.

You might wonder what the purpose of `components` is. For the sake of this
blog post, let us just say that it is a place for user-defined things not
directly related to HTTP.

The following type requires the request to have *at least* a body of type
`String`, and headers of type `StrMap String`. The request can have more
fields, this type does not care. The response and components are not
constrained at all by this type.

```haskell
forall req res c.
Conn { body :: String
     , headers :: StrMap String
     | req
     }
     res
     c
```

The second building block, middleware, are simply functions transforming
connection values. They take a pure connection, and return another connection
inside some type `m`, where `m` is usually an Applicative or a Monad.

```haskell
type Middleware m c c' = c -> m c'
```

As middleware are monadic functions, just as the computations used with
[Bind][bind], they compose using [Kleisli composition][kleisli-composition].

```haskell
-- Compose three middleware functions sequentially,
-- from left to right:
authenticateUser >=> parseForm >=> saveTodo
```

As `m` is a parameter of Conn, you can customize the middleware chain to use
a monad stack for tracking state, providing configuration, gathering metrics,
and much more.

## Response State Transitions

In Hyper, the state of a response is tracked in its type. This guarantees
correctness in response handling, preventing incorrect ordering of headers
and body writes, incomplete responses, or other similar mistakes.

The contract of state transitions is encoded in a type class implemented by
response writers for specific servers. This makes it possible to write reusable
functions on top of the protocol that can run with different server
implementations.

To safe a few keystrokes, and your innocent eyes, let us begin by looking at
the type alias for middleware transitioning between response states.

```haskell
type ResponseStateTransition m rw from to =
  forall req res c.
  Middleware
  m
  (Conn req {writer :: rw from | res} c)
  (Conn req {writer :: rw to | res} c)
```

Now, on to the encoding of state transitions, the `ResponseWriter` type class.

```haskell
class ResponseWriter rw m b | rw -> b where
  writeStatus
    :: Status
    -> ResponseStateTransition m rw StatusLineOpen HeadersOpen

  writeHeader
    :: Header
    -> ResponseStateTransition m rw HeadersOpen HeadersOpen

  closeHeaders
    :: ResponseStateTransition m rw HeadersOpen BodyOpen

  send
    :: b
    -> ResponseStateTransition m rw BodyOpen BodyOpen

  end
    :: ResponseStateTransition m rw BodyOpen ResponseEnded
```

I know, it looks a bit scary with all the types. Stay strong, or have a look at
it rendered as a state diagram instead.

{% plantuml The possible transitions between response states. %}
@startuml
!include styles.iuml

state StatusLineOpen
state HeadersOpen
state BodyOpen
state ResponseEnded

[*] -right-> StatusLineOpen
StatusLineOpen --> HeadersOpen : writeStatus
HeadersOpen --> HeadersOpen : writeHeader
HeadersOpen --> BodyOpen : closeHeaders
BodyOpen --> BodyOpen : send
BodyOpen --> ResponseEnded : end
ResponseEnded -right-> [*]

@enduml
{% endplantuml %}

We can write a middleware, based on the state transition functions of the type
class, that responds friendly to all requests.

``` haskell
writeStatus (Tuple 200 "OK")
>=> writeHeader (Tuple "Content-Type" "text/plain")
>=> closeHeaders
>=> write "Greetings, friend!"
>=> end
```

Say we forget the line with `closeHeaders`. What do we get? An informative type
error, of course!

```haskell
Could not match type

  HeadersOpen

with type

  BodyOpen
```

There are easier-to-use functions written on top of the response API so that
you do not have to write out all state transitions explicitly.

``` haskell
writeStatus statusOK
>=> contentType textHTML
>=> closeHeaders
>=> respond (div [] [ h1 [] [ text "Greetings!" ]
                    , p [] [ text "You are most welcome." ]
                    ])
```

The `respond` function has the added benefit of decoupling the response type,
in this case `HTML`, from the response type required by the server, which
for NodeJS is a `Buffer`. The transformation between `HTML` and `Buffer` is
done behind the scenes by a bit of type class machinery.

The bits presented so far constitute the low level API for responding to HTTP
requests in Hyper. I have plans for designing a simpler interface, based on
*Ring* response maps in Clojure, where response handlers simply return a data
structure describing the response to be written. Such an API can be built on
top of the existing low-level API.

## Wrapping Up

We have looked at the core design of Hyper, and the motivation behind the
project, but merely scratched the surface. The [documentation][documentation]
describes in much greater detail the implementation and current components of
Hyper. I hope, however, that I have caught your interest. If so, please go
ahead and check out some of the provided examples at GitHub:

* [Hello Hyper][hello-hyper]
* [Form Parser][form-parser]
* [File Server][file-server]
* [Authentication][authentication]
* [Authentication and Authorization][authentication-and-authorization]
* [Type-safe Resource Routing][routing]

Also, note that the project is highly experimental, not nearly ready for any
production use. But it could be! If you are interested in contributing, do not
hesitate to send me a tweet or a PM. I need help writing the library,
middleware, servers for different PureScript backends, more examples,
documentation, etc. If you want to have a look at the source code, it's [also
on GitHub][repository].

Thanks for reading!

[documentation]: https://owickstrom.github.io/hyper/
[repository]: https://github.com/owickstrom/hyper
[bind]: https://pursuit.purescript.org/packages/purescript-prelude/2.1.0/docs/Control.Bind#v:(%3E=%3E)
[kleisli-composition]: https://pursuit.purescript.org/packages/purescript-prelude/2.1.0/docs/Control.Bind#v:(%3E=%3E)
[hello-hyper]: https://github.com/owickstrom/hyper/blob/master/examples/hello-hyper/Main.purs
[form-parser]: https://github.com/owickstrom/hyper/blob/master/examples/form-parser/Main.purs
[file-server]: https://github.com/owickstrom/hyper/blob/master/examples/file-server/Main.purs
[authentication]: https://github.com/owickstrom/hyper/blob/master/examples/authentication/Main.purs
[authentication-and-authorization]: https://github.com/owickstrom/hyper/blob/master/examples/authentication-and-authorization/Main.purs
[routing]: https://github.com/owickstrom/hyper/blob/master/examples/routing/Main.purs
