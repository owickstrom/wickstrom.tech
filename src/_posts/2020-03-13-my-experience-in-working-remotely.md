---
layout: post
title:  "My Experiences in Working Remotely"
author: Oskar Wickström
date: 2020-03-13
categories: general
tags: ["remote", "work", "technology"]
---

As I'm writing this, COVID-19 is spreading across the globe. Companies
partly or fully shut down their offices, and have people work from
their homes. Others might self-isolate by choice. In any case, you
might now find yourself in a situation where your team is suddenly a
remote-working team.

I've been working remotely for the last 3 years. It's been a great
privilege and pleasure, but not without its issues. There's a bunch of
challenges that I've gone through, and I'm very much still learning.

In this post I'll focus on _my personal experiences_. I won't go into
tips on everyday routines, suggestions on tooling, or how to convince
your boss. I don't have any quick advice, no evenly-numbered list of
things you should do each day. But I'm hoping this post will give you
some inspiration and useful considerations when setting out on your
remote work journey.

## Why Remote

First, I'm an introvert with a limited capacity for social
interaction. I love hanging out with people, and I consider myself
socially competent, whatever that means. But social interaction drains
me quickly. The work I do, and the way I prefer doing it, requires a
lot of concentration. There are definitely social elements that
require face-to-face interaction, but the majority of my work
activities involve thinking, communicating, designing, and the
occasional pressing of buttons.

Remote work is thus a huge enabler for prolonged focus and
productivity in my life. Working from home, I'm able to get into the
zone. This is something that never happens in a big open-plan
office. I may get work done, but I'm not nearly as focused as when I'm
alone.

Working remotely also supports the way of life I prefer. There's a
myriad of possibilities opening up when you go remote. However, I'll
save those for another day. Check out [Chris Herd's twitter
account](https://twitter.com/chris_herd) for more inspiration.

You might be in a remote-working situation for other reasons, perhaps
because of COVID-19. It might be temporary, something you get through,
and long back for normal co-located work. Or you might be in this for
the long run. Whatever your situation, if you are working remotely,
fully or partly, why not make the best of it?

## Asynchronous Written Communication

As mentioned before, a big part of my productivity and happiness is
being able to concentrate. This means no interruptions. It doesn't,
however, mean no _communication_. Communication is key in my
work. It's not only about organization and scheduling, but also
communicating ideas and design, giving feedback on documents, and
reviewing code.

That's why I prefer teams that favor _asynchronous_
communication. Everyone communicates when they have time and energy,
like before or after breaks, and doesn't expect immediate replies.

![[This Is Why You Shouldn't Interrupt a
Programmer](https://heeris.id.au/2013/this-is-why-you-shouldnt-interrupt-a-programmer/)</br>
&copy; Jason Heeris 2013 [CC-BY-NC-ND](https://creativecommons.org/licenses/by-nc-nd/2.5/se/)
](/assets/interrupt-programmer.png)

In the best case, synchronous communication requires a lot of, well,
synchronization. You have to agree on times for conference calls or
mob programming sessions, and if it's co-located, agree on where to
meet. In the worst case, it's plain interruption. Still, I do enjoy
the occasional pair-programming session.

Hand-in-hand with asynchronous communication goes _writing_. While I
can send my colleagues a voice mail, the more realistic scenario is
that I write to them. What medium I choose depends on the purpose, the
contents, and the audience:

* *Direct message:* This works well for small nudges, e.g. "Hey, I
  couldn't find any recent commits on your branch, are you sure you've
  pushed?" There's no use in anyone else reading this message, so why
  not keep the noise down. In many cases, though, what gets sent as a
  direct message should be a public message.
* *Public channel message:* Be sure to set up a low-friction
  communication channel for the entire team. This is great for all
  kinds of updates and questions in everyday work. You never know who
  can or has time to answer a question, or who needs to know what you
  know. Err on the side of sharing too much information. Also, use a
  tool where the messages are persistent, as you might want to go back
  and search the history.
  
  I realize that there's a psychological safety aspect to team
  communication. Everyone in your team might not feel confident and
  safe in posting or commenting publicly. I can't give much good
  advice on this topic, so I'll just leave it as an important thing to
  consider.
* *Shared document:* This is the more heavy-weight option, and I use
  it for project documentation, design documents, and lengthy feedback
  on others' work.
  
  It's the higher-friction choice, so why not send a public channel
  message? For me, this is mostly about stress. In most modern office
  suites, commenting is built in, and reviewers can add feedback in
  their own time. Even better, if inline comments are supported, small
  threads of discussion are localized to the relevant parts of the
  document. If discussion happens in a chat system, it feels like
  everything flies by, and you have to respond within the hour, or the
  moment will be lost forever.
  
  Further, it's much easier to catalog and browse documents than to
  search chat history. Important design decisions can be read many
  years after they were taken, acting as an [architecture decision
  log](https://github.com/joelparkerhenderson/architecture_decision_record).
  
  Recently, I've introduced a light-weight RFC process for our team's
  design documentation. If you're interested, check out [the meta-RFC
  and template](/assets/meta.rfc.pdf).
* *Email:* I try to use this only for communication outside the
  team. It's the common denominator of communication in many
  organizations. I find email terrible for persistent many-to-many
  communication, especially to give feedback on documents.

Reconfiguring yourself and your team from synchronous to asynchronous
communication structures can be challenging. You'll need to work with
this actively, constantly nudging each other in the right
direction. You might need to get comfortable with new tools, improve
your writing skills, and be more careful in how you express yourself
and interpret others. I tend to read in too much of a serious tone in
written communication, thinking that people are discontent or
otherwise unhappy. Written communication is hard.

## Keeping Focused

One concern about remote work is how to stay focused. After having
worked remotely for a few years, I've learned a lot about myself. In
my spare time, like 95% of it, I'm doing something that I consider
productive. But I don't feel stressed about it, and my focus is
high. When work is motivating, it feels the same, and I generally have
little trouble keeping focused.

Thus, I've been searching for what motivates me. After a bunch of
detours, I've learned that it's not Haskell, functional programming,
or any other technology. It's not about things being hard or complex,
or colleagues being smart or having brilliant academic or industry
track records.

For me, it's working with respectful and inspiring colleagues on a
shared vision, and achieving observable results. Working on products
that'll inspire and help others in their work or daily life. Things
that fit with my views on politics and society.

The worst experiences I've had, remote or on-site, have been either
toy projects that would never see any real use, or projects with toxic
management or overall culture. Those projects have often been very
interesting in terms of technology, but to no avail.

For you, motivation might be something _completely different_. I'm not
saying you should do exactly what I did. Find your own priorities and
ways of keeping focused. It takes practice.

## Distance in Time and Space

In remote work, you can be distanced not only geographically, but also
by time zones. Both kinds of distancing pose challenges.

Except for times like these, with self-isolation measures to slow down
COVID-19, I think it's imperative that you meet your team face-to-face
regularly. Time zone difference implies geographical distance, and
geographical distance makes it harder and more demanding to meet in
person. Depending on your personal or family situation, long-distance
or long-duration travel might not be an option.

Time zone difference in itself can be problematic. Much of your
communication gets interrupted and delayed, with replies not being
available until the next morning. You might be tempted or forced to
stay longer at work, or begin earlier, depending on the offset.

I've struggled with feelings of isolation and depression due to time
zone difference. If you're alone in your time zone, it can be hard to
maintain a feeling of connection and well-being. Conversely, I've
worked with geographical distance, but in the same time zone, and felt
very connected and energized.

Be careful about distance. Find ways to stay connected.

## The Amplifier

A major thing I've learned about remote work is that is functions as
an amplifier of my feelings.

In good projects, working part or full-time remote amplifies the good
aspects of the project. A good day of working remotely is often way
better than a good day in the open-plan office. Conversely, in bad
projects, remote work makes everything ten times worse. On a bad day
working remotely, I'm all alone in the void. I have no comfort, no
shoulder to lean on, no one to have a quick coffee with and get things
off my chest. I've tried co-working spaces, but the water cooler
conversations just didn't do it for me.

I'm not saying it's impossible to support each other online, but I
think it's harder. This is the main reason I need to meet my
colleagues face-to-face, even if it's not very often. In bad projects
I've been in, being on-site and having colleagues to lean on has been
my only safety net.

## Find Your Own Way

If you're anything like me, and if you're going to work remotely by
your own choice, carefully pick your projects and teams. Both the risk
and the reward are amplified.

If you find yourself thrown in to remote work, find ways of improving
your team's communication and collaboration. In many regards, it's not
the same as co-located work. Find your own setup, don't blindly follow
this or any other post. Then, I'm sure you'll enjoy it!

Good luck, and stay safe!

_If you have any comments, please reply to [this Twitter
thread](https://twitter.com/owickstrom/status/1238408428256743424)._
