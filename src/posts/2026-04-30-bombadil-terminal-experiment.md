---
title: "The Bombadil Terminal Experiment"
date: "April 30, 2026"
author: "Oskar Wickström"
---

Last week at [Bug Bash 2026](https://antithesis.com/bugbash/conference2026/), I
had a bunch of interesting discussions about testing non-web interfaces with
[Bombadil](http://bombadil.bot/), our new property-based testing framework for
user interfaces. One direction that I already wanted to explore is *terminal user
interfaces* (TUIs), and the hallway discussions gave me a nudge to get going. I
started hacking on the flight back home, and a few days later that embryo of a
TUI fuzzer started to emerge.

![The fuzzer in action, finding a bug in vitetris. (CW: flashing!)](/assets/tetris-stuck.mp4)

It's built on top of two key crates:

1. [portable-pty](https://crates.io/crates/portable-pty), a pseudo-teletype in
Rust that runs the program under test, and
2. [libghostty-vt](https://crates.io/crates/libghostty-vt), a Rust wrapper
around the Zig library, which interprets the output of the PTY and provides
a virtual terminal API from which you can read cell contents, styles, scroll
through the scrollback, etc.

With these two in place, I built a *very* basic fuzzer for TUIs: it runs the
command you give it, polls its output, and writes interleaved random input
sequences (printable ASCII characters and ANSI escape sequences). It also
scrolls and resizes the terminal occasionally. Timing is a bit tricky, but it
seems the current approach works fine: polling reads until the terminal is
idle, capture state, then apply new inputs. Regarding speed, it depends a lot
on the program being tested, but it looks capable of capturing at least 300
states per second.

I tried finding some basic TUI programs and terminal games to test. Much to my surprise,
within the first few days I had found four seemingly real bugs in real
software:

* [vitetris](https://github.com/vicgeralds/vitetris) has a bug where if you enter just a number in the host name (e.g.
`6`) and try to connect to a remote game, the UI freezes.
* [btop](https://github.com/aristocratos/btop) has two different bugs, [one recently
fixed](https://github.com/aristocratos/btop/issues/1010) that I confirmed
fixed with the latest version (1.4.6), and [one that I just
reported](https://github.com/aristocratos/btop/issues/1627). Both were triggered
by this fuzzer.
* [rlwrap](https://github.com/hanslub42/rlwrap) got into a segfault which I haven't yet been able to troubleshoot.

Pretty cool. Today, I merged this work to `main` in Bombadil. It's not yet
released, but if you're curious you can try it already by downloading a
`bombadil-terminal` binary from the
[CI](https://github.com/antithesishq/bombadil/actions/workflows/ci.yml)
artifacts. On macOS you'll need to remove the quarantine bit to bypass GateKeeper.

Now, the work remains to make this a solid tool. Here are some future goals:

* Integrate it with the specification framework in Bombadil, so that you can
  define custom properties and action generators. It'd be neat to provide an
  API akin to `querySelector` that could parse and traverse panels drawn with
  box-drawing characters. You probably also want to validate that those borders
  line up correctly.
* Generate a lot more diverse input and terminal actions. For instance,
  generate sequences from the [Kitty keyboard
  protocol](https://sw.kovidgoyal.net/kitty/keyboard-protocol/).
* Make the test runner's user interface better. Perhaps a TUI?!
* Make this part of the ordinary `bombadil` binary, I think. There could be
  subcommands for `browser` and `terminal` testing tools.
* Run it in Antithesis to see what that fuzzer can find.

All right, short post today --- I just wanted to share my excitement and early results.

*A huge thanks to [Uzair Aftab](https://github.com/Uzaaft), maintainer of [libghostty-rs](https://github.com/Uzaaft/libghostty-rs), for helping me get libghostty-vt building under Nix!*
