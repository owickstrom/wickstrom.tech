---
layout: post
title:  "A Faster Test Workflow for Haskell"
date:   2016-04-19 07:00 +0200
categories: Programming
tags: ["haskell", "tools"]
---

The amount of tests in Oden has been increasing steadily since the beginning of
this compiler iteration. I like to have tests run automatically when both
library and test source code changes, so I have previously used *nodemon*
to watch for file changes and execute `cabal test` or the like. It was starting
to get a bit slow so decided to search for a new setup.

As I already use *tmux* for all work I figured I could use that together with
GHCi somehow.  Tmux has a command called `send-keys` that, unsurpringsly, lets
you programmatically send keys as though they were typed in at the target pane.

{% highlight bash %}
tmux send-keys -t "mysession:0.1" "echo hello, world" Enter
{% endhighlight %}

This sends `echo hello, world` followed by a linebreak to the pane with index
1, in the window with index 0, in the session called *mysession*. The `-t`
switch handles convenient tokens such as `last` and `left` so you don't have to
figure out indices. See the [tmux man
page](http://man.openbsd.org/OpenBSD-current/man1/tmux.1) for all variants.

I also use *tmuxinator* to setup windows and panes for my projects, which means
I know what indices they will have. The following is an excerpt of my new Oden
tmuxinator configuration.

{% highlight yaml %}
name: oden
root: ~/Projects/oden-lang

windows:
  - oden:
      root: ~/Projects/oden-lang/oden
      panes:
        - vim
        - cabal repl spec
  - test-watch:
      root: ~/Projects/oden-lang/oden
      panes:
        - >
            nodemon \
              --watch src \
              --watch test \
              -e hs \
              --exec 'tmux send-keys -t "oden:0.1" :r Enter main Enter'
{% endhighlight %}

This tmuxinator configuration launches one split window with `vim` and `cabal
repl spec` as well as another window running `nodemon`. When Haskell source
files in `src` and `test` change the GHCi REPL in the first window gets the
`:r` command executed, which reloads all modules, followed by the invocation of
`main` which runs the tests again.

With this I have hundreds of tests run automatically in about a second, which
I think is fine for Haskell. My previous setup had a latency at about 4
seconds. All in all, I'm very happy with this workflow.

If you have any improvements or recommendations you would like to share then
please post a comment below.
