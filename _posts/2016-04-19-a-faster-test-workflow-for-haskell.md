---
layout: post
title:  "A Faster Test Workflow for Haskell"
date:   2016-04-19 07:00 +0200
author: Oskar Wickström
categories: Programming
tags: ["haskell", "tools"]
excerpt: "This post shows a way of running large Haskell test suites quickly using tmux and the send-keys command."
---

The amount of tests in Oden has been increasing steadily since the beginning of
this compiler iteration. I like to have tests run automatically when both
library and test source code changes, so I have previously used *nodemon*
to watch for file changes and execute `cabal test` or the like. It was starting
to get a bit slow so decided to search for a new setup. This post explains
how it works.

## Sendings Keys

As I already use *tmux* for all work I figured I could use that together with
GHCi somehow. Tmux has a command called `send-keys` that lets you
programmatically send keys as though they were typed in at the target pane.

```bash
tmux send-keys -t "mysession:0.1" "echo hello, world" Enter
```

This sends `echo hello, world` followed by a line break to the pane with index
1, in the window with index 0, in the session called *mysession*. The `-t`
switch handles convenient tokens such as `last` and `left` so you don't have to
figure out indices. See the [tmux man
page](http://man.openbsd.org/OpenBSD-current/man1/tmux.1) for all variants.

I also use *tmuxinator* to setup windows and panes for my projects, which means
I know what indices they will have. The following is an excerpt of my new Oden
tmuxinator configuration.

```yaml
name: oden
root: ~/Projects/oden-lang

windows:
  - oden:
      root: ~/Projects/oden-lang/oden
      panes:
        - vim
        - cabal exec ghci -- -isrc -itest Main
  - test-watch:
      root: ~/Projects/oden-lang/oden
      panes:
        - >
            nodemon \
              --watch src \
              --watch test \
              -e hs \
              --exec 'tmux send-keys -t "oden:0.1" :r Enter main Enter'
```

This tmuxinator configuration launches one split window with Vim and GHCi as
well as another window running `nodemon`. When Haskell source files in `src`
and `test` change the GHCi REPL in the first window gets the `:r` command
executed, which reloads all modules, followed by the invocation of `main` which
runs the tests again.

The reason I use `cabal exec -- -isrc -itest Main` is to have both targets
loaded as interpreted sources in GHCi. When you run `cabal repl <target>` only
that target's sources can be reloaded. I want to reload changed source files
both in the library and in the test targets.

If you are using [Stack](https://github.com/commercialhaskell/stack) just
replace `cabal` with `stack` in these commands and it should work as well.

## Summary

With this I have hundreds of tests run automatically in about a second, which
I think is fine for Haskell. My previous setup had a latency at about 4
seconds. All in all, I'm very happy with this workflow.

If you have any improvements or recommendations you would like to share then
please post a comment below.

## Update 2016-07-31

Even though it is somewhat slower, I use `stack test --file-watch` more often
in the beginning of a project as it handles changes in the Cabal file. I have
also started using the interactive session support in the Emacs *haskell-mode*
to reload test modules using `C-c C-l` and then run the individual tests with
`hspec spec` in the Emacs GHCi session. Maybe later I will create an Emacs
keybinding for reloading a test module and running the specs.
