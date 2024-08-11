---
title:  "A Flexible Minimalist Neovim for 2024"
date: Aug 12, 2024
author: Oskar Wickström
---

* In search of ...
* I've been through a bunch:
    - Adobe Dreamweaver
    - Sublime Text
    - Atom
    - Vim
    - IntelliJ IDEA
    - VS Code
    - Emacs
* And now, I'm back with Neovim.
* LazyVim
    - startup times
    - distractions
    - bloated; constant stream of updates for plugins I didn't even realized I had
    - complex setup, didn't care to learn how all the magic worked
* I've also evaluated Zed recently, but it hasn't quite stuck. It seems to insist on being a "global" application. I want it to be local to each project, with the correct PATH from direnv, and I want multiple instances to run simultaneously.
* Decided to do a Neovim setup from scratch
* Goals:
    - keep plugins to an absolute minimum
        - coding
        - writing
    - minimalist UI
    - fast startup time
    - preferably managed by home-manager
* home-manager module code snippet
    - enable, vimAlias, extraPackages
* plugins
    - nvim-lspconfig
        - LSP is builtin, but lspconfig configures Neovim for use with various language servers
    - nvim-treesitter.withPlugins
        - languages I use right now (might add and remove over time)
        - no "nice to haves"
    - conform-nvim
        - auto-formatting is useful and I don't want to think about it
    - neogit
        - magit was the reason I clung to Emacs for so long
        - neogit is, for my purposes, as good
        - enabled me to finally switch
    - fzf-vim
        - I use two commands, `:Files` and `:GFiles`, as a quick jump-to-file
        - this is perhaps the one plugin I could do without, writing some smaller helper around `fzf`
        - on the other hand, I'm trying to start using `:Buffers` instead of stumbling around with `:bnext` and `:bprev`, so it'll possibly stay around
* plugins I don't use
    - completions
        - file name completions with regular C-x C-f
        - omnicomplete rebound to C-Space
        - buffer completions with regular C-x C-n
        - spelling with C-x C-s (or z=)
    - snippets
        - I've not found much use for them
        - most time is spent reading or editing existing code, not churning out new boilerplate
        - might make more sense for things like HTML, but I don't write raw HTML often
* colorscheme
    - quiet
    - small tweaks (show code)
* config structure
    - vimscript entrypoint (based on my old immortal base config)
    - Lua scripts for plugins
    - use `vim.loader.enable()` to improve startup time
    - maybe everything could be Lua, but I don't care that much
    - it's also nice to have the base config (without plugins) portable, can just copy-paste onto a server or other temporary environment
* summary
    - minimal
    - startup time
    - I understand all of it
    - perhaps I spend more time on it than I save? (XKCD link) but I'm happy
    - let's see how long it lasts        
