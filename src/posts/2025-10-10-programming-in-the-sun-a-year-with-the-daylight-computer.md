---
title: "Programming in the Sun: A Year with the Daylight Computer" 
date: "October 10, 2025"
author: "Oskar Wickström"
---

I've been [hinting](https://x.com/owickstrom/status/1976247186632683564)
[on](https://x.com/owickstrom/status/1866979819961135555)
[X/Twitter](https://x.com/owickstrom/status/1927349072693694743) about my use
of the Daylight DC-1 as a programming environment, and after about a year of
use, it's time to write about it in longer form. This isn't a full product
review, but rather an experience report on coding in sunlight. It's also about
the Boox Tab Ultra -- which has a different type of display -- and how it
compares to the DC-1 for my use cases.

This is _not_ a sponsored post.

![Neovim in Termux on the Daylight DC-1.](/assets/sun-light-mode/daylight-3.webp)

Why do I even bother, you might ask? Sunlight makes me energetic and alert,
which I need when I work. Living in the Nordics, 50% of the year is primarily
dark, so any direct daylight I can get becomes really important. I usually run
light mode on my Framework laptop during the day, but working in actual
daylight with these displays, or plain old paper, is even better. 

## The Setup 

Here are the main components of this coding environment:

* [Daylight DC-1](https://daylightcomputer.com/product): an Android-based tablet with a "Live Paper" display (Reflective LCD, not E-Ink)
* [8BitDo Retro Mechanical Keyboard](https://www.8bitdo.com/retro-mechanical-keyboard/): a mechanical Bluetooth-enabled keyboard, with Kailh key switches and USB-C charging and optional connection
* [Termux](https://play.google.com/store/apps/details?id=com.termux&hl=sv&pli=1): a terminal emulator for Android, with a package collection based on `apt`
* SSH, tmux, and Neovim: nothing surprising here

I use a slimmed-down version of my [regular
dotfiles](https://github.com/owickstrom/home-manager), because this setup
doesn't use Nix. I've manually installed Neovim, tmux, and a few other
essentials, using the package manager that comes with Termux. I've configured
Termux to not show its virtual keyboard when a physical keyboard is connected
(the Bluetooth keyboard). The Termux theme is "E-Ink" and the font is JetBrains
Mono, all built into Termux. Neovim uses the built-in `quiet` colorscheme for
maximum contrast.

Certain work requires a more capable environment, and in those cases I connect
to my workstation using SSH and run tmux in there. For writing or simpler
programming projects (I've even done Rust work with Cargo, for instance), the
local Termux environment is fine.

Sometimes I want to go really minimalist, so I hide the tmux status bar and run
[`Goyo`](https://github.com/junegunn/goyo.vim) in Neovim. _Deep breaths. Feel
the fresh air in your lungs._ This is especially nice for writing blog posts
like this one.

![Minimalist typing with Goyo in Neovim.](/assets/sun-light-mode/daylight-2.webp)

My blog editing works locally in Termux, with a live reloading Chrome in a
split window, here during an evening writing session with the warm backlight
enabled:

![Split-screen blogging locally on the Daylight.](/assets/sun-light-mode/daylight-6.webp)

There's the occasional Bluetooth connection problem with the 8BitDo keyboard. I
also don't love the layout, and I'm considering getting the [Kinesis Freestyle2
Blue](https://kinesis-ergo.com/shop/freestyle2-blue-pc/) instead. I already
have the wired version for my workstation, and the ergonomics are great.

## Daylight DC-1 vs Boox Tab Ultra

What about the Boox? I've had this device for longer and I really like it too,
but not for the same tasks. The E-Ink display is, quite frankly, a lot nicer to
read on; EPUB books, research PDFs, web articles, etc. The 227 PPI instead of
the Daylight's 190 PPI makes a difference, and I like the look of E-Ink better
overall.

However, the refresh rate and ghosting make it a bit frustrating for typing.
Same goes for drawing, which I've used the Daylight for a lot. Most of my home
renovation blueprints are sketched on the Daylight. The refresh rate makes it
possible.

When reading at night with a more direct bedside lamp, often in combination
with a subtle backlight, the Boox is much better. The Daylight screen can
glare quite a bit, so the only option is backlight only. And at that point, 
a lot of the paperlike quality goes away.

You can also get some glare when there's direct sunlight at a particular
angle:

![You may get glare in direct sunlight or from lamps at some angles.](/assets/sun-light-mode/daylight-1.webp)

Even if I don't write or program directly on the Boox, I've experimented with
using it as a secondary display, like for the live reload blog preview:

![Using the Boox Tab Ultra as a secondary display by browsing the live reload HTTP server.](/assets/sun-light-mode/boox-1.webp)

To sum up, these devices are good for different things, in my experience. I've
probably spent more time on the Boox, because I've had it for longer and I've
read a lot on it, but the Daylight has been much better for typing and drawing.

Another thing I'd like to try is a larger E-Ink monitor for my workstation,
like [the one Zack is hacking
on](https://x.com/zack_overflow/status/1952445830541291916). I'm hoping this
technology continues to improve on refresh rate, because I love E-Ink. Until
then, the Daylight is a good compromise.

![Touch grass, as they say.](/assets/sun-light-mode/daylight-4.webp)

