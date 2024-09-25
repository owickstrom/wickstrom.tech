---
title:  "A Flexible Minimalist Neovim for 2024"
date: Aug 12, 2024
author: Oskar Wickström
---

<picture>
    <source type="image/webp" srcset="/assets/nvim.webp">
    <img src="/assets/nvim.png" alt="Neovim welcome screen">
</picture>

In the eternal search of a better text editor, I've recently gone back to Neovim.
I've taken the time to configure it myself, with as few plugins and other cruft as possible.
My goal is a minimalist editing experience, tailored for exactly those tasks that I do regularly, and nothing more.
In this post, I'll give a brief tour of my setup and its motivations.

Over the years, I've been through a bunch of editors.
Here are most of them, in roughly chronological order:

- Adobe Dreamweaver
- Sublime Text
- Atom
- Vim/Neovim
- IntelliJ IDEA
- VS Code
- Emacs

The majority were used specifically for the work I was doing at the time.
VS Code for web development, IntelliJ IDEA for JVM languages, Emacs for Lisps. 
Vim and Emacs have been the most generally applicable, and the ones that I've enjoyed the most.

I've also evaluated Zed recently, but it hasn't quite stuck. 
The start-up time and responsiveness is impressive.
However, it seems to insist on being a _global_ application.
I want my editor to be local to each project, with the correct `PATH` from direnv, and I want multiple isolated instances to run simultaneously.
Maybe I'll revisit it later.

## Returning to Neovim

Yeah, so I'm back in Neovim.
I actually started with the [LazyVim](http://www.lazyvim.org/) distribution based on recommendation.
On the positive side, it got me motivated to use Neovim again.
But I had some frustrations with the distribution.

The start-up time wasn't great.
I guess it did some lazy loading of plugins to speed things up, but the experience was still that of an IDE taking its time to get ready.
Not the Neovim experience I was hoping for.

More importantly, it was full of distractions; popups, status messages, news, and plugins I didn't need.
I guess it takes a batteries-included approach.
That might make sense for beginners and those just getting into Neovim, but I realized quickly that I wanted something different.

Supposedly I could strip things out, but instead I decided to start from scratch and build the editor I wanted.
One that I understand.
Joran Dirk Greef talks about two different types of artists, [sculptors and painters](https://www.youtube.com/watch?v=w3WYdYyjek4&ab_channel=TigerBeetle), and this is an exercise in painting.

More concretely, my main goals are:

Plugins

: I want to keep plugins to an absolute minimum.
  My editor is meant for coding and writing, and for what I work on right now. 
  I might add or remove plugins and configuration over time, and that's fine.

User interface

: It should be as minimalist as I can make it.
  Visual distractions kept at a minimum.
  This includes busy colorschemes, which I find add little value.
  A basic set of typographical conventions in monochrome works well for me.

Start-up time

: With the way I use Neovim, it needs to start fast.
  I quit and start it all the time, jumping between directories, working on different parts of the system or a project.
  Often I put it in the background with <kbd>C-z</kbd>, but not always.
  Making it faster seems to be mainly an exercise in minimizing plugins.

## I have to mention Nix, you know?

I manage dotfiles and other personal configuration using Nix and home-manager.
The Neovim configuration is no exception, so I'll include some of the Nix bits as well.

The `vim.nix` module declares that I want Neovim installed and exposed as `vim`:

```nix
programs.neovim = {
  enable = true;
  vimAlias = true;
  ...
};
```

In that attribute set, there are two other important parts; the `plugins` list and the `extraConfig`.

## Plugins

Let's start with the plugins:

```nix
plugins = with pkgs.vimPlugins; [
  nvim-lspconfig
  (nvim-treesitter.withPlugins(p: [
    p.bash
    p.json
    p.lua
    p.markdown
    p.nix
    p.python
    p.rust
    p.zig
    p.vimdoc
  ]))
  conform-nvim
  neogit
  fzf-vim
];
```

Basically it's five plugins, not counting the various treesitter parsers:

nvim-lspconfig
: LSP is included in Neovim nowadays, but it doesn't know about specific language servers.
  The `lspconfig` plugins helps with configuring Neovim for use with various servers.

nvim-treesitter
: This provides better highlighting for Neovim. I've only included the languages I use right now.
  No nice-to-haves. I could probably remove this plugin, but I haven't tried yet.

conform-nvim
: Auto-formatting is useful and I don't want to think about it.
  Of course, I could run `:%!whatever-formatter`, but I'd rather have the editor do it for me.

neogit
: Magit was the reason I clung to Emacs for so long.
  Neogit is, for my purposes, a worthy replacement.
  It enabled me to finally make the switch.

fzf-vim
: A wrapper around the awesome [fzf](https://github.com/junegunn/fzf) fuzzy finder.
  I use `:Files` and `:GFiles`, as quick jump-to-file commands (think `C-p` in VS Code or Zed).
  They are bound to `<Leader>ff` and `<Leader>gf`, respectively.
  This might be another plugin I could do without, writing a small helper around `fzf`, or just making do with `:find` and `**/` wildcards.
  On the other hand, I'm trying out `:Buffers` instead of stumbling around with `:bnext` and `:bprev`.

One great thing with the Nix setup is I don't need a package manager in Neovim itself.

## Batteries Are Included

Many things I don't need plugins for.
For instance, there are a ton of plugins for auto-completion, but Neovim has most of that built in, and I prefer triggering it manually:

  * File name completions with `C-x C-f`
  * Omnicomplete (rebound to `C-Space` in my case)
  * Buffer completions with `C-x C-n`
  * Spelling suggestions with `C-x C-s` or `z=`

I've tried various snippet engines many times, but not found them very useful.
Most of my time is spent reading or modifying existing code, not churning out new boilerplate.
Instead they tend to clutter the auto-completion list.
Snippets might make more sense for things like HTML, but I don't write HTML often, and in that case I'd prefer some emmet/zen-coding plugin.

You can get great mileage from learning how to use the Quickfix list.
I'm no expert, but I prefer investing in composable primitives that I can reuse in different ways.
Project-wide search-and-replace is such an example:

```vim
:grep whatever
:cfdo s/whatever/something else/g | update
:cfdo :bd
```

Here we search (`:grep`, which I've configured to use `rg`), substitute and save each file, and delete those buffers afterwards.

I also use `:make` and `:compiler` a lot. Neovim is cool.

## Life in Monochrome

Maybe I'm just growing old, but I prefer a monochrome colorscheme.
Right now I'm using the built-in `quiet` scheme with a few extra tweaks:

```vimscript
set termguicolors
set bg=dark
colorscheme quiet
highlight Keyword gui=bold
highlight Comment gui=italic
highlight Constant guifg=#999999
highlight NormalFloat guibg=#333333
```

It's black-and-white, but keywords are bold, comments are darker and italic, and literals are light gray. 
Here's how it looks with some Zig code:

<picture>
    <source type="image/webp" srcset="/assets/nvim-monochrome.webp">
    <img src="/assets/nvim-monochrome.png" alt="Neovim in monochrome with the quiet colorscheme">
</picture>

Maybe I'm coming off as nostalgic or conservative, but I _do_ find it more readable this way.

Another thing I'm going to try soon is writing on the [Daylight Computer](https://daylightcomputer.com/), hopefully in Neovim.
Being comfortable with a monochrome colorscheme should come in handy.

## The Full Configuration

My config uses a Vimscript entrypoint (`extraConfig` in the Nix code).
This part is based on my near-immortal config from the good old Vim days.
Early on, it calls `vim.loader.enable()` to improve startup time.
I use Lua scripts for configuring the plugins and related keymap bindings.
Maybe everything could be Lua, but I haven't gotten that far yet.
However, it's nice to have the base config somewhat portable; I can just copy-paste it onto a server or some other temporary environment and have a decent `/usr/bin/vim` experience.

You'll find the full configuration in [nix.vim](https://github.com/owickstrom/home-manager/blob/master/vim.nix) and the Lua bits inside the [vim/](https://github.com/owickstrom/home-manager/blob/master/vim/) directory.

That's about it!
I'm really happy with how fast and minimalistic it is.
It starts in just above 100ms.
And I can understand all of my configuration (even if I don't understand all of Neovim.)
Perhaps I've [spent more time on it than I've saved](https://xkcd.com/1205/), but at least I'm happy so far.


I'm writing and publishing this on my birthday.
What a treat to find time to blog on such an occasion!
