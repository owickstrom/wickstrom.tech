---
layout: post
title:  "Paramount Color Scheme for Vim"
date:   2016-05-15 09:00 +0200
author: Oskar Wickström
categories: Programming
tags: ["vim", "tools"]
---

Having tried a lot of color schemes for editors, especially for Vim, I have
gotten quite picky. All right, *very* picky. Most of the time Vim has been
configured to use [Tomorrow Night][] or [Gruvbox][]. Although they're great,
they have felt a bit over the top. Also, depending on where I'm sitting at the
moment I use both dark and light backgrounds for best contrast.

[Tomorrow Night]: https://github.com/chriskempson/tomorrow-theme
[Gruvbox]: https://github.com/morhetz/gruvbox

The last couple of days I've tried [off][] with some small
modifications for accent colors, e.g. number, strings and escape sequences.
This setup fit my taste very well so I decided to package that as a color
scheme for Vim. I call it [Paramount][].

[Paramount]: https://github.com/owickstrom/vim-colors-paramount

The goal of Paramount is to not clutter your editor with all colors of the
rainbow, just to keep it simple. It uses three monochrome colors for most text
together with a purple accent color. Diffs and some errors use red and green
colors.

Paramount is based on the [pencil][] and [off][] color schemes. Thanks for the
great work on those projects!

[pencil]: https://github.com/reedes/vim-colors-pencil
[off]: https://github.com/pbrisbin/vim-colors-off

## Screenshots

The following screenshots show some Go code together with the *Latin Modern
Mono* font on light and dark backgrounds.

![](https://raw.githubusercontent.com/owickstrom/vim-colors-paramount/master/screenshots/latin-modern-light.png)
![](https://raw.githubusercontent.com/owickstrom/vim-colors-paramount/master/screenshots/latin-modern-dark.png)

...and if you use the *Monaco* font to show the Ruby code from
[vimcolors.com](http://vimcolors.com/):

![](https://raw.githubusercontent.com/owickstrom/vim-colors-paramount/master/screenshots/monaco-light.png)
![](https://raw.githubusercontent.com/owickstrom/vim-colors-paramount/master/screenshots/monaco-dark.png)

## Installation

Simply copy [the color scheme
file](https://raw.githubusercontent.com/owickstrom/vim-colors-paramount/master/colors/paramount.vim)
to your `~/.vim/colors directory` or use a plugin manager like Plug or Vundle
and add `"owickstrom/vim-colors-paramount"` as a plugin.

The source is available [on
GitHub](https://github.com/owickstrom/vim-colors-paramount).  You can preview
the theme on the [*~/.vim/colors*](http://vimcolors.com/438/paramount/dark)
site.
