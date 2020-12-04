---
layout: post
title: Declarative GTK+ Programming with Haskell
author: Oskar Wickström
categories: programming
tags: ["haskell", "gtk", "gui", "declarative", "functional"]
excerpt: |
  Born out of the need in FastCut, a screencast video editor that I'm writing,
  the gi-gtk-declarative packages combines declarative GUI programming with
  GTK+. This post outlines the motivation and introduces the packages and their
  uses.
---

This post introduces a declarative GTK+ architecture for Haskell which I've
been working on during the journey with FastCut, a video editor specialized for
my own screencast editing workflow. It outlines the motivation, introduces
the packages and their uses, and highlights parts of the implementation.

## Imperative GUI Programming

When starting to work on _FastCut_, I wanted to use a GUI framework that would
allow FastCut to be portable across Linux, macOS, and Windows. Furthermore, I
was looking for a native GUI framework, as opposed to something based on web
technology. GTK+ stood out as an established framework, with good bindings for
Haskell in the [haskell-gi] package.

It didn't take me very long before the imperative APIs of GTK+ became
problematic. Widgets are created, properties are set, callbacks are attached,
and methods are called, all in `IO`. With the imperative style, I see there
being two distinct phases of a widget's life cycle, which need to be handled
separately in your rendering code:

1. *construction*, where you instantiate the widget with its initial state,
   and
2. *subsequent updates*, where some relevant state changes and the widget's
   state is modified to reflect that. Often, the application state and the
   widget state are the same.

This programming style makes testing and locally reasoning about your code
really hard, as it forces you to scatter your application logic and state over
`IORef`s, `MVar`s, and various callbacks, mutating shared state in `IO`. These
symptoms are not specific to GTK+ or its Haskell bindings, but are common in
object-oriented and imperative GUI frameworks. If you're familiar with
JavaScript and jQuery UI programming, you have most likely experienced similar
problems.

While GTK+ has the [Glade] "WYSIWYG" editor, and the `Builder` class, they only
make the first phase declarative. You still need to find all widgets in the
instantiated GUI, attach event handlers, and start mutating state, to handle
the second phase.

After having experienced these pains and getting repeatedly stuck when building
FastCut with the imperative GTK+ APIs, I started exploring what a declarative
GTK+ programming model could look like.

## Going Declarative

The package I've been working on is called _gi-gtk-declarative_, and it aims to
be a minimal declarative layer on top of the GTK+ bindings in [haskell-gi].

Rendering becomes a pure function from state to a _declarative widget_, which
is a data structure _representation_ of the user interface. The library uses a
patching mechanism to calculate the updates needed to be performed on the
actual GTK+ widgets, based on differences in the declarative widgets, similar
to a [virtual DOM](https://reactjs.org/docs/faq-internals.html) implementation.

Event handling is declarative, i.e. you declare which events are emitted on
particular signals, rather than mutating state in callbacks or using
concurrency primitives to communicate changes. Events of widgets can be mapped
to other data types using the `Functor` instance, making widgets reusable and
composable.

Finally, gi-gtk-declarative tries to be agnostic of the application
architecture it's used in. It should be possible to reuse it for different
styles. As we shall see later there is a state reducer-based architecture
available in _gi-gtk-declarative-app-simple_, and FastCut is using a custom
architecture based on indexed monads and the [Motor] library.

## Declarative Widgets

By reusing type-level information provided by the `haskell-gi` framework, and
by using the `OverloadedLabels` language extension in GHC, gi-gtk-declarative
can support many widgets automatically. Even though some widgets need special
support, specifically containers, it is a massive benefit not having to
redefine all GTK+ widgets.

### Single Widgets

A single widget, i.e. one that cannot contain children, is constructed using
the `widget` smart constructor, taking a GTK+ widget constructor and an
attribute list:

```haskell
myButton = widget Button []

myCheckButton = widget CheckButton []
```

Note that [Button] and [CheckButton] are constructors from
gi-gtk. They are not defined by gi-gtk-declarative.

### Bins

_Bins_, in GTK+ lingo, are widgets that contain a single child. They are
created using the `bin` smart constructor:

```haskell
myScrollArea =
  bin ScrolledWindow [] $
    widget Button []
```

Other examples of bins are [Expander], [Viewport], and [SearchBar].

### Containers

Containers are widgets that can contain zero or more child widgets, and they
are created using the `container` smart constructor:

```haskell
myListBox =
  container ListBox [] $ do
    bin ListBoxRow [] $ widget Button []
    bin ListBoxRow [] $ widget CheckButton []
```

In regular GTK+, containers often accept any type of widget to be added to the
container, and if the container requires its children to be of a specific
type, it will automatically insert the in-between widget implicitly. An example
of such a container is `ListBox`, which automatically wraps added children in
`ListBoxRow` bins, if needed.

In gi-gtk-declarative, on the other hand, containers restrict the type of
their children to make these relationships explicit. Thus, as seen above, to
embed child widgets in a `ListBox` they have to be wrapped in `ListBoxRow`
bins.

Another example, although slightly different, is `Box`. While `Box` doesn't
have a specific child widget type, you can in regular GTK+ add children using
the `boxPackStart` and `boxPackEnd` methods. The arguments to those methods are
_expand_, _fill_, and _padding_, which control how the child is rendered
(_packed_) in the box. As gi-gtk-declarative doesn't support method calls,
there is a helper function and corresponding declarative widget `boxChild` to
control `Box` child rendering:

```haskell
myBox =
  container Box [] $ do
    boxChild False False 0 $ widget Button []
    boxChild True True 0 $ widget CheckButton []
```

Note that we are using `do` notation to construct adjacent `boxChild` markup
values. There is a monadic `MarkupOf` builder in the library that the
`container` smart constructor takes as its last argument. Although we need
the `Monad` instance to be able to use do notation, the return value of such
expressions are rarely useful, and is thus constrained to `()` by the library.

## Attributes

All declarative widgets can have attributes, and so far we've only seen empty
attribute lists. Attributes on declarative widgets are not the same as GTK+
_properties_. They do include GTK+ properties, but also include CSS classes
declarations and event handling.

### Properties

One type of attribute is a _property_ declaration. To declare a property, use
the `(:=)` operator, which takes a property name label, and a property value,
much like `(:=)` in [haskell-gi]:

```haskell
myButtonWithLabel =
  widget Button [#label := "Click Here"]

myHorizontallyScrolledWindow =
  bin ScrolledWindow [ #hscrollbarPolicy := PolicyTypeAutomatic ] $
    someSuperWideWidget

myContainerWithMultipleSelection =
  container ListBox [ #selectionMode := SelectionModeMultiple ] $
    children
```

To find out what properties are available, see the
[GI.Gtk.Objects](https://hackage.haskell.org/package/gi-gtk-3.0.24/docs/GI-Gtk-Objects.html)
module, find the widget module you're interested in, and see the "Properties"
section. As an example, you'd find the properties available for `Button`
[here](https://hackage.haskell.org/package/gi-gtk-3.0.24/docs/GI-Gtk-Objects-Button.html#g:32).

### Events

Using the `on` attribute, you can emit events on GTK+ signal
emissions.

```haskell
counterButton clickCount =
  let msg = "I've been clicked "
            <> Text.pack (show clickCount)
            <> " times."
  in widget
      Button
      [ #label := msg
      , on #clicked ButtonClicked
      ]
```

Some events need to be constructed with `IO` actions, to be able to query
underlying GTK+ widgets for attributes. The `onM` attribute receives the widget
as its first argument, and returns an `IO event` action. In the following
example `getColorButtonRgba` has type `ColorButton -> IO (Maybe RGBA)`, and so
we compose it with an `fmap` of the `ColorChanged` constructor to get an `IO
Event`.

```haskell
data Event = ColorChanged (Maybe RGBA)

colorButton color =
  widget
  ColorButton
  [ #title := "Selected color"
  , #rgba := color
  , onM #colorSet (fmap ColorChanged . getColorButtonRgba)
  ]
```

You can think of `onM` having the following signature, even if it's really a
simplified version:

```{.haskell}
onM
  :: Gtk.SignalProxy widget
  -> (widget -> IO event)
  -> Attribute widget event
```

Finally, CSS classes can be declared for widgets in the attributes list, using
the `classes` attribute:


```{.haskell}
myAnnoyingButton =
  widget
    Button
    [ classes ["big-button"]
    , #label := "CLICK ME"
    ]
```

## GI.Gtk.Declarative.App.Simple

In addition to the declarative widget library gi-gtk-declarative, there's an
application architecture for you to use, based on the state reducer design of
[Pux].

At the heart of this architecture is the `App`:

```haskell
data App state event =
  App
    { update :: state -> event -> Transition state event
    , view   :: state -> Widget event
    , inputs :: [Producer event IO ()]
    , initialState :: state
    }
```

The type parameters `state` and `event` will be instantiated with our specific
types used in our application. For example, if we were writing a "Snake" clone,
our state datatype would describe the current playing field, the snake
length and where it's been, the edible objects' positions, etc. The event
datatype would likely include key press events, such as "arrow down" and
"arrow right".

The `App` datatype consists of:

* an `update` function, that reduces the current state and a new event to a
  `Transition`, which decides the next state to transition to,
* a `view` function, that renders a state value as a `Widget`, parameterized by
  the `App`s event type,
* inputs, which are [Producer]s that feed events into the application, and
* the initial state value of the state reduction loop.

### Running Applications

To run an `App`, you can use the convenient `run` function, that initializes
GTK+ and sets up a window for you:

```haskell
run
  :: Typeable event
  => Text                 -- ^ Window title
  -> Maybe (Int32, Int32) -- ^ Optional window size
  -> App state event      -- ^ Application to run
  -> IO ()
```

There's also `runInWindow` if you like to initialize GTK+ yourself, and set
up your own window; something you need to do if you want to use CSS, for
instance.

## Declarative "Hello, world!"

The [haskell-gi] README includes an "Hello, world!" example, written in an
imperative style:

```haskell
main :: IO ()
main = do
  Gtk.init Nothing

  win <- new Gtk.Window [ #title := "Hi there" ]

  on win #destroy Gtk.mainQuit

  button <- new Gtk.Button [ #label := "Click me" ]

  on button #clicked $
    set
      button
      [ #sensitive := False
      , #label := "Thanks for clicking me"
      ]

  #add win button

  #showAll win

  Gtk.main
```

It has two states; either the button has not been clicked yet, in which it
shows a "sensitive" button, or the button has been clicked, in which it shows
an "insensitive" button and a label thanking the user for clicking.

<table style="width: 100%; text-align: center;">
  <tr>
    <td>
![Initial state of "Hello, world!"](/assets/gtk/hello1.png){width=248}
    </td>
    <td>
![Terminal state of "Hello, world!"](/assets/gtk/hello2.png){width=248}
    </td>
  </tr>
</table>

Let's rewrite this application in a declarative style, using gi-gtk-declarative
and gi-gtk-declarative-app-simple, and see how that works out! Our state and
event datatypes describe what states the application can be in, and what events
can be emitted, respectively:

```haskell
data State = NotClicked | Clicked

data Event = ButtonClicked
```

Our view function, here defined as `view'`, renders a label according to what
state the application is in:

```haskell
view' :: State -> Widget Event
view' = \case
  NotClicked ->
    widget
      Button
      [ #label := "Click me"
      , on #clicked ButtonClicked
      ]
  Clicked ->
    widget
      Button
      [ #sensitive := False
      , #label := "Thanks for clicking me"
      ]
```

The `update` function reduces the current state and an event to a `Transition
event state`, which can either be `Transition` or `Exit`. Here we always
transition to the `Clicked` state if the button has been clicked.

```haskell
update' :: State -> Event -> Transition State Event
update' _ ButtonClicked = Transition Clicked (return Nothing)
```

Note that the `Transition` constructor not only takes the next state, but also
an `IO (Maybe Event)` action. This makes it possible to generate a new event in
the update function.

Finally, we run the "Hello, world!" application using `run`.

```haskell
main :: IO ()
main =
  run
    "Hi there"
    Nothing
    App
    { view = view'
    , update = update'
    , inputs = []
    , initialState = NotClicked
    }
```

Comparing with the imperative version, I like this style a lot better. The
rendering code is a pure function, and core application logic can also be pure
functions on data structures, instead of mutation of shared state. Moreover,
the small state machine that was hiding in the original code is now explicit
with the `State` sum type and the `update'` function.

There are more examples in [gi-gtk-declarative] if you want to check them out.

## Implementation

Writing gi-gtk-declarative has been a journey full of insights for me, and
I'd like to share some implementation notes that might be interesting and
helpful if you want to understand how the library works.

### Patching

At the core of the library lies the `Patchable` type class. The `create` method
creates a new GTK+ widget given a declarative `widget`. The `patch` method
calculates a minimal patch given two declarative widgets; the old and the new
version:

```haskell
class Patchable widget where
  create :: widget e -> IO Gtk.Widget
  patch :: widget e1 -> widget e2 -> Patch
```

A patch describes a modification of a GTK+ widget, specifies a replacement of a
GTK+ widget, or says that the GTK+ widget should be kept as-is.

```haskell
data Patch
  = Modify (Gtk.Widget -> IO ())
  | Replace (IO Gtk.Widget)
  | Keep
```

Replacing a widget is necessary if the declarative widget changes from one type
of widget to another, say from `Button` to `ListBox`. We can't modify a
`Button` to become a `ListBox` in GTK+, so we have to create a new GTK+ widget
and replace the existing one.

### Heterogeneous Widgets

Declarative widgets are often wrapped in the `Widget` datatype, to support
widgets of any type to be used as a child in a heterogeneous container, and to
be able to return _any_ declarative widget, as we did in the `App` view function
previously. The `Widget` datatype is a [GADT]:

```haskell
data Widget event where
  Widget
    :: ( Typeable widget
       , Patchable widget
       , Functor widget
       , EventSource widget
       )
    => widget event
    -> Widget event
```

If you look at the `Widget` constructor's type signature, you can see that it
hides the inner `widget` type, and that it carries all the constraints needed
to write instances for patching and event handling.

We can define a `Patchable` instance for `Widget` as the inner widget is
constrained to have an instance of `Patchable`. As the `widget` is also
constrained with `Typeable`, we can use `eqT` to compare the types of two
`Widget`s. If their inner declarative widget types are equal, we can calculate
a patch from the declarative widgets. If not, we replace the old GTK+ widget
with a new one created from the new declarative widget.

```haskell
instance Patchable Widget where
  create (Widget w) = create w
  patch (Widget (w1 :: t1 e1)) (Widget (w2 :: t2 e2)) =
    case eqT @t1 @t2 of
      Just Refl -> patch w1 w2
      _         -> Replace (create w2)
```

Similar to the case with `Patchable`, as we've constrained the inner widget
type in the GADT, we can define instances for `Functor` and `EventSource`.

At first, it might seem unintuitive to use dynamic typing in Haskell, but I
think this case is very motivating, and it's central to the implementation of
gi-gtk-declarative.

### Smart Constructors and Return Type Polymorphism

All smart constructors --- `widget`, `bin`, and `container` --- can return
either a `Widget` value, such that you can use it in a context where the inner
widget type needs to be hidden, or a `MarkupOf` with a type specifically needed
in the contexts in which the widget is used, for example, a bin or container
with a requirement on what child widget it can contain.

Here are some possible specializations of smart constructor return types:

```haskell
widget Button [] :: Widget MyEvent
widget Button [] :: MarkupOf (SingleWidget Button) MyEvent ()

bin ScrolledWindow [] _ :: Widget MyEvent
bin ScrolledWindow [] _ :: MarkupOf (Bin ScrolledWindow Widget) MyEvent ()

container Box [] _ :: Widget MyEvent
container Box [] _ :: MarkupOf (Container Box (Children BoxChild)) MyEvent ()
```

As a small example, consider the helper `textRow` that constructs a `ListBoxRow`
to be contained in a `ListBox`:

```haskell
myList :: Widget Event
myList =
  container ListBox [] $
    mapM textRow ["Foo", "Bar", "Baz"]

textRow :: Text -> MarkupOf (Bin ListBoxRow Widget) Event ()
textRow t =
  bin ListBoxRow [] $
    widget Label [ #label := t ]
```

As the type signature above shows, the `textRow` function returns a `MarkupOf`
value parameterized by a specific child type: `Bin ListBoxRow Widget`. You can
read the whole type as "markup containing bins of list box rows, where the list
box rows can contain any type of widget, and where they all emit events of type
`Event`." I know, it's a mouthful, but as you probably won't split your
markup-building function up so heavily, and as GHC will be able to infer these
types, it's not an issue.

The return type polymorphism of the smart constructors should not affect type
inference badly. If you find a case where it does, please submit an issue on
GitHub.

## Summary

Callback-centric GUI programming is hard. I prefer using data structures and
pure functions for core application code, and keep it decoupled from the GUI
code by making rendering as simple as a function `State -> Widget`.  This is
the ideal I'm striving for, and what motivated the creation of these packages.

I have just released
[gi-gtk-declarative](https://hackage.haskell.org/package/gi-gtk-declarative)
and
[gi-gtk-declarative-app-simple](https://hackage.haskell.org/package/gi-gtk-declarative-app-simple)
on Hackage. They are both to be regarded as experimental packages, but I hope
for them to be useful and stable some day.  Please try them out, and post
issues on the [GitHub
tracker](https://github.com/owickstrom/gi-gtk-declarative/issues) if you find
anything weird, and give me shout if you have any questions.

The gi-gtk-declarative library is used heavily in FastCut, with great
success. Unfortunately that project is not open source yet, so I can't point
you to any code examples right now. Hopefully, I'll have it open-sourced soon,
and I'm planning on blogging more about its implementation.

Until then, happy native and declarative GUI hacking!

[haskell-gi]: https://github.com/haskell-gi/haskell-gi
[Glade]: https://glade.gnome.org/
[Motor]: http://hackage.haskell.org/package/motor
[Button]: http://hackage.haskell.org/package/gi-gtk-3.0.24/docs/GI-Gtk-Objects-Button.html#t:Button
[CheckButton]: http://hackage.haskell.org/package/gi-gtk-3.0.24/docs/GI-Gtk-Objects-CheckButton.html#t:CheckButton
[Expander]: http://hackage.haskell.org/package/gi-gtk-3.0.24/docs/GI-Gtk-Objects-Expander.html#t:Expander
[Viewport]: http://hackage.haskell.org/package/gi-gtk-3.0.24/docs/GI-Gtk-Objects-Viewport.html#t:Viewport
[SearchBar]: http://hackage.haskell.org/package/gi-gtk-3.0.24/docs/GI-Gtk-Objects-SearchBar.html#t:SearchBar
[Pux]: http://purescript-pux.org/
[Producer]: http://hackage.haskell.org/package/pipes-4.3.9/docs/Pipes-Core.html#t:Producer
[gi-gtk-declarative]: https://github.com/owickstrom/gi-gtk-declarative
[GADT]: https://en.wikibooks.org/wiki/Haskell/GADT
[Data.Dynamic]: http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Dynamic.html
[Typeable]: http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Typeable.html#t:Typeable
