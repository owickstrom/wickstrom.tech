

### Techniques Not Covered

There are variations of PBT that I won't cover in this series, but
that I can highly recommend learning more about:

* _Model-based testing_ is used to test stateful systems. By
  implementing a simplified model of the system you're testing, and
  running a sequence of commands and queries on both the model and the
  actual system, you can detect discrepancies between their observed
  behaviors. You might find errors in both your model and in your actual
  implementation. As with regular PBT, you end up with both a higher
  confidence in your implementation, and with an executable
  specification of your system.
  
  If you want to learn more about model-based testing, begin by checking
  out [Experiences with QuickCheck:Testing the Hard Stuff and Staying
  Sane](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf)
  by John Hughes. It begins with a simple example of testing a circular
  buffer, and proceeds with case studies from Volvo and Klarna using
  model-based testing on more complex systems.
  
* _Data-driven property testing_ is an alternative to generative PBT,
  where you have a known database of input values to check properties
  against. We used this technique at my previous job, together with a
  legacy implementation of the system acting as a _test oracle_. I wrote
  the article [Testing Our Ruby and Haskell Implementations
  Side-By-Side](https://blog.mpowered.team/posts/2018-testing-ruby-haskell-implementations.html)
  covering that project in detail.
