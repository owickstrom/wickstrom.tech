---
title: "Time Travelling and Fixing Bugs with Property-Based Testing"
author: Oskar Wickström
date: November 17, 2019
---

Property-based testing (PBT) is a powerful testing technique that
helps us find edge cases and bugs in our software. A challenge in
applying PBT in practice is coming up with useful properties. This
tutorial is based on a simple but realistic system under test (SUT),
aiming to show some ways you can test and find bugs in such logic
using PBT. It covers refactoring, dealing with non-determinism,
testing generators themselves, number of examples to run, and coupling
between tests and implementation. The code is written in Haskell and
the testing framework used is
[Hedgehog](http://hackage.haskell.org/package/hedgehog).

This tutorial was originally written as a book chapter, and later
extracted as a standalone piece. Since I'm not expecting to finish the
PBT book any time soon, I decided to publish the chapter here.

## System Under Test: User Signup Validation

The business logic we'll test is the validation of a website's user
signup form. The website requires users to sign up before using the
service. When signing up, a user must pick a valid username. Users
must be between 18 and 150 years old.

Stated formally, the validation rules are:

[$$
\begin{aligned}
0 \leq \text{length}(\text{name}) \leq 50 \\
18 \leq \text{age} \leq 150
\end{aligned}
\qquad(1)$$]{#eq:validation-rules}

The signup and its validation is already implemented by previous
programmers. There have been user reports of strange behaviour, and
we're going to locate and fix the bugs using property tests.

Poking around the codebase, we find the data type representing the form:

``` {.haskell .numbers}
data SignupForm = SignupForm
  { formName  :: Text
  , formAge   :: Int
  } deriving (Eq, Show)
```

And the existing validation logic, defined as `validateSignup`. We won't
dig into to the implementation yet, only its type signature:

``` {.haskell .numbers}
validateSignup
  :: SignupForm -> Validation (NonEmpty SignupError) Signup
```

It's a pure function, taking `SignupForm` data as an argument, and
returning a `Validation` value. In case the form data is valid, it
returns a `Signup` data structure. This data type resembles `SignupForm`
in its structure, but refines the age as a `Natural` when valid:

``` {.haskell .numbers}
data Signup = Signup
  { name  :: Text
  , age   :: Natural
  } deriving (Eq, Show)
```

In case the form data is invalid, `validateSignup` returns a non-empty
list of `SignupError` values. `SignupError` is a union type of the
possible validation errors:

``` {.haskell .numbers}
data SignupError
  = NameTooShort Text
  | NameTooLong Text
  | InvalidAge Int
  deriving (Eq, Show)
```

### The Validation Type

The `Validation` type comes from the
[validation](https://hackage.haskell.org/package/validation) package.
It's parameterized by two types:

1.  the type of validation failures
2.  the type of a successfully validated value

The `Validation` type is similar to the
[Either](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Either.html#t:Either)
type. The major difference is that it *accumulates* failures, rather
than short-circuiting on the first failure. Failures are accumulated
when combining multiple `Validation` values using `Applicative`.

Using a non-empty list for failures in the `Validation` type is common
practice. It means that if the validation fails, there's at least one
error value.

## Validation Property Tests

Let's add some property tests for the form validation, and explore the
existing implementation. We begin in a new test module, and we'll need a
few imports:

``` {.haskell .numbers}
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text          (Text)
import           Data.Validation
import           Hedgehog
import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range
```

Also, we'll need to import the implementation module:

``` {.haskell .numbers}
import Validation
```

We're now ready to define some property tests.

### A Positive Property Test

The first property test we'll add is a *positive* test. That is, a test
using only valid input data. This way, we know the form validation
should always be successful. We define
`prop_valid_signup_form_succeeds`:

``` {.haskell .numbers}
prop_valid_signup_form_succeeds = property $ do
  let genForm = SignupForm <$> validName <*> validAge ➊
  form <- forAll genForm ➋

  case validateSignup form of ➌
    Success{}        -> pure ()
    Failure failure' -> do
      annotateShow failure'
      failure
```

First, we define `genForm` (➊), a generator producing form data with
valid names and ages. Next, we generate `form` values from our defined
generator (➋). Finally, we apply the `validateSignup` function and
pattern match on the result (➌):

-   In case it's successful, we have the test pass with `pure ()`
-   In case it fails, we print the `failure'` and fail the test

The `validName` and `validAge` generators are defined as follows:

``` {.haskell .numbers}
validName :: Gen Text
validName = Gen.text (Range.linear 1 50) Gen.alphaNum

validAge :: Gen Int
validAge = Gen.integral (Range.linear 18 150)
```

Recall the validation rules (eq. 1). The ranges in these generators
yielding valid form data are defined precisely in terms of the
validation rules.

The character generator used for names is `alphaNum`, meaning we'll only
generate names with alphabetic letters and numbers. If you're
comfortable with regular expressions, you can think of `genValidName` as
producing values matching `[a-zA-Z0-9]+`.

Let's run some tests:

``` {.hedgehog}
λ> check prop_valid_signup_form_succeeds
  ✓ <interactive> passed 100 tests.
```

Hooray, it works.

### Negative Property Tests

In addition to the positive test, we'll add *negative* tests for the
name and age, respectively. Opposite to positive tests, our negative
tests will only use invalid input data. We can then expect the form
validation to always fail.

First, let's test invalid names.

``` {.haskell .numbers}
prop_invalid_name_fails = property $ do
  let genForm = SignupForm <$> invalidName <*> validAge ➊
  form <- forAll genForm

  case validateSignup form of ➋
    Failure (NameTooLong{}  :| []) -> pure ()
    Failure (NameTooShort{} :| []) -> pure ()
    other                          -> do ➌
      annotateShow other
      failure
```

Similar to our the positive property test, we define a generator
`genForm` (➊). Note that we use `invalidName` instead of
`validName`.

Again, we pattern match on the result of applying `validateSignup`
(➋). In this case we expect failure. Both `NameTooLong` and
`NameTooShort` are expected failures. If we get anything else, the
test fails (➌).

The test for invalid age is similar, expect we use the `invalidAge`
generator, and expect only `InvalidAge` validation failures:

``` {.haskell .numbers}
prop_invalid_age_fails = property $ do
  let genForm = SignupForm <$> validName <*> invalidAge
  form <- forAll genForm
  case validateSignup form of
    Failure (InvalidAge{} :| []) -> pure ()
    other                        -> do
      annotateShow other
      failure
```

The `invalidName` and `invalidAge` generators are also defined in terms
of the validation rules (eq. 1), but with ranges ensuring no overlap
with valid data:

``` {.haskell .numbers}
invalidName :: Gen Text
invalidName =
  Gen.choice [mempty, Gen.text (Range.linear 51 100) Gen.alphaNum]

invalidAge :: Gen Int
invalidAge = Gen.integral (Range.linear minBound 17)
```

Let's run our new property tests:

``` {.hedgehog}
λ> check prop_invalid_name_fails
  ✓ <interactive> passed 100 tests.

λ> check prop_invalid_age_fails
  ✓ <interactive> passed 100 tests.
```

All good? Maybe not. The astute reader might have noticed a problem with
one of our generators. We'll get back to that later.

### Accumulating All Failures

When validating the form data, we want *all* failures returned to the
user posting the form, rather than returning only one at a time. The
`Validation` type accumulates failures when combined with `Applicative`,
which is exactly what we want. Yet, while the hard work is handled by
`Validation`, we still need to test that we're correctly combining
validations in `validateSignup`.

We define a property test generating form data, where all fields are
invalid (➊). It expects the form validation to fail, returning two
failures (➋).

``` {.haskell .numbers}
prop_two_failures_are_returned = property $ do
  let genForm = SignupForm <$> invalidName <*> invalidAge ➊
  form <- forAll genForm
  case validateSignup form of
    Failure failures | length failures == 2 -> pure () ➋
    other -> do
      annotateShow other
      failure
```

This property is weak. It states nothing about *which* failures should
be returned. We could assert that the validation failures are equal to
some expected list. But how do we know if the name is too long or too
short? I'm sure you'd be less thrilled if we replicated all of the
validation logic in this test.

Let's define a slightly stronger property. We pattern match, extract
the two failures (➊), and check that they're not equal (➋).

``` {.haskell .numbers}
prop_two_different_failures_are_returned = property $ do
  let genForm = SignupForm <$> invalidName <*> invalidAge
  form <- forAll genForm
  case validateSignup form of
    Failure (failure1 :| [failure2]) -> ➊
      failure1 /== failure2 ➋
    other                            -> do
      annotateShow other
      failure
```

We're still not being specific about which failures should be returned.
But unlike `prop_two_failures_are_returned`, this property at least
makes sure there are no duplicate failures.

## The Value of a Property

Is there a faulty behaviour that would slip past
`prop_two_different_failures_are_returned`? Sure. The implementation
could have a typo or copy-paste error, and always return `NameTooLong`
failures, even if the name is too short. Does this mean our property is
bad? Broken? Useless?

In itself, this property doesn't give us strong confidence in the
correctness of `validateSignup`. In conjuction with our other
properties, however, it provides value. Together they make up a stronger
test suite.

Let's look at it in another way. What are the *benefits* of weaker
properties over stronger ones? In general, weak properties are
beneficial in that they are:

1.  easier to define
2.  likely to catch simple mistakes early
3.  less coupled to the SUT

A small investment in a set of weak property tests might catch a lot of
mistakes. While they won't precisely specify your system and catch the
trickiest of edge cases, their power-to-weight ratio is compelling.
Moreover, a set of weak properties is better than no properties at all.
If you can't formulate the strong property you'd like, instead start
simple. Lure out some bugs, and improve the strength and specificity of
your properties over time.

Coming up with good properties is a skill. Practice, and you'll get
better at it.

## Testing Generators

Remember how in [Negative Property Tests](#negative-property-tests) we
noted that there's a problem? The issue is, we're not covering all
validation rules in our tests. But the problem is not in our property
definitions. It's in one of our *generators*, namely `genInvalidAge`.
We're now in a perculiar situation: we need to test our tests.

One way to test a generator is to define a property specifically testing
the values it generates. For example, if we have a generator `positive`
that is meant to generate only positive integers, we can define a
property that asserts that all generated integers are positive:

``` {.haskell .numbers}
positive :: Gen Int
positive = Gen.integral (Range.linear 1 maxBound)

prop_integers_are_positive = property $ do
  n <- forAll positive
  assert (n >= 1)
```

We could use this technique to check that all values generated by
`validAge` are valid. How about `invalidAge`? Can we check that it
generates values such that all boundaries of our validation function are
hit? No, not using this technique. Testing the correctness of a
generator using a property can only find problems with *individual*
generated values. It can't perform assertions over *all* generated
values. In that sense, it's a *local* assertion.

Instead, we'll find the generator problem by capturing statistics on
the generated values and performing *global* assertions. Hedgehog, and
a few other PBT frameworks, can measure the occurences of user-defined
*labels*. A label in Hedgehog is a `Text` value, declared with an
associated condition. When Hedgehog runs the tests, it records the
percentage of tests in which the condition evaluates to `True`. After
the test run is complete, we're presented with a listing of
percentages per label.

We can even have Hedgehog fail the test unless a certain percentage is
met. This way, we can declare mininum coverage requirements for the
generators used in our property tests.

### Adding Coverage Checks

Let's check that we generate values covering enough cases, based on the
validation rules in eq. 1 . In `prop_invalid_age_fails`, we use `cover`
to ensure we generate values outside the boundaries of valid ages. 5% is
enough for each, but realistically they could both get close to 50%.

``` {.haskell .numbers}
prop_invalid_age_fails = property $ do
  let genForm = SignupForm <$> validName <*> invalidAge
  form <- forAll genForm
  cover 5 "too young" (formAge form <= 17)
  cover 5 "too old"   (formAge form >= 151)
  case validateSignup form of
    Failure (InvalidAge{} :| []) -> pure ()
    other                        -> do
      annotateShow other
      failure
```

Let's run some tests again.

``` {.hedgehog}
λ> check prop_invalid_age_fails
  ✗ <interactive> failed
    after 100 tests.
    too young 100% ████████████████████ ✓ 5%
  ⚠ too old     0% ···················· ✗ 5%

       ┏━━ test/Validation/V1Test.hs ━━━
    63 ┃ prop_invalid_age_fails = property $ do
    64 ┃   let genForm = SignupForm <$> validName <*> invalidAge
    65 ┃   form <- forAll genForm
    66 ┃   cover 5 "too young" (formAge form <= 17)
    67 ┃   cover 5 "too old"   (formAge form >= 151)
       ┃   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       ┃   │ Failed (0% coverage)
    68 ┃   case validateSignup form of
    69 ┃     Failure (InvalidAge{} :| []) -> pure ()
    70 ┃     other                        -> do
    71 ┃       annotateShow other
    72 ┃       failure

    Insufficient coverage.
```

100% too young and 0% too old. The `invalidAge` generator is clearly not
good enough. Let's have a look at its definition again:

``` {.haskell .numbers}
invalidAge :: Gen Int
invalidAge = Gen.integral (Range.linear minBound 17)
```

We're only generating invalid ages between the minimum bound of `Int`
and `17`. Let's fix that, by using `Gen.choice` and another generator
for ages greater than 150:

``` {.haskell .numbers}
invalidAge :: Gen Int
invalidAge = Gen.choice
  [ Gen.integral (Range.linear minBound 17)
  , Gen.integral (Range.linear 151 maxBound)
  ]
```

Running tests again, the coverage check stops complaining. But there's
another problem:

``` {.hedgehog}
λ> check prop_invalid_age_fails
  ✗ <interactive> failed at test/Validation/V1Test.hs:75:7
    after 3 tests and 2 shrinks.
    too young 67% █████████████▎······ ✓ 5%
  ⚠ too old    0% ···················· ✗ 5%

       ┏━━ test/Validation/V1Test.hs ━━━
    66 ┃ prop_invalid_age_fails = property $ do
    67 ┃   let genForm = SignupForm <$> validName <*> invalidAge
    68 ┃   form <- forAll genForm
       ┃   │ SignupForm { formName = "a" , formAge = 151 }
    69 ┃   cover 5 "too young" (formAge form <= 17)
    70 ┃   cover 5 "too old"   (formAge form >= 151)
    71 ┃   case validateSignup form of
    72 ┃     Failure (InvalidAge{} :| []) -> pure ()
    73 ┃     other                        -> do
    74 ┃       annotateShow other
       ┃       │ Success Signup { name = "a" , age = 151 }
    75 ┃       failure
       ┃       ^^^^^^^
```

OK, we have an actual bug. When the age is 151 or greater, the form is
deemed valid. It should cause a validation failure. Looking closer at
the implementation, we see that a pattern guard is missing the upper
bound check:

``` {.haskell .numbers}
  validateAge age' | age' >= 18 = Success (fromIntegral age')
                   | otherwise  = Failure (pure (InvalidAge age'))
```

If we change it to `age' >= 18 && age' <= 150`{.haskell .numbers}, and
rerun the tests, they pass.

``` {.hedgehog}
λ> check prop_invalid_age_fails
  ✓ <interactive> passed 100 tests.
    too young 53% ██████████▌········· ✓ 5%
    too old   47% █████████▍·········· ✓ 5%
```

We've fixed the bug.

Measuring and declaring requirements on coverage is a powerful tool in
Hedgehog. It gives us visibility into the generative tests we run,
making it practical to debug generators. It ensures our tests meet our
coverage requirements, even as implementation and tests evolve over
time.

## From Ages to Birth Dates

So far, our efforts have been successful. We've fixed real issues in
both implementation and tests. Management is pleased. They're now
asking us to modify the signup system, and use our testing skills to
ensure quality remains high.

Instead of entering their age, users will enter their birth date. Let's
suppose this information is needed for something important, like sending
out birthday gifts. The form validation function must be modified to
check, based on the supplied birth date date, if the user signing up is
old enough.

First, we import the `Calendar` module from the `time` package:

``` {.haskell .numbers}
import           Data.Time.Calendar
```

Next, we modify the `SignupForm` data type to carry a `formBirthDate` of
type `Date`, rather than an `Int`.

``` {.haskell .numbers}
data SignupForm = SignupForm
  { formName      :: Text
  , formBirthDate :: Day
  } deriving (Eq, Show)
```

And we make the corresponding change to the `Signup` data type:

``` {.haskell .numbers}
data Signup = Signup
  { name      :: Text
  , birthDate :: Day
  } deriving (Eq, Show)
```

We've also been requested to improve the validation errors. Instead of
just `InvalidAge`, we define three constructors for various invalid
birthdates:

``` {.haskell .numbers}
data SignupError
  = NameTooShort Text
  | NameTooLong Text
  | TooYoung Day
  | TooOld Day
  | NotYetBorn Day
  deriving (Eq, Show)
```

Finally, we need to modify the `validateSignup` function. Here, we're
faced with an important question. How should the validation function
obtain *today's date*?

### Keeping Things Deterministic

We could make `validateSignup` a non-deterministic action, which in
Haskell would have the following type signature:

``` {.haskell .numbers}
validateSignup
  :: SignupForm -> IO (Validation (NonEmpty SignupError) Signup)
```

Note the use of `IO`. It means we could retrieve the current time from
the system clock, and extract the `Day` value representing today's date.
But this approach has severe drawbacks.

If `validateSignup` uses `IO` to retrieve the current date, we can't
test it with other dates. What it there's a bug that causes validation
to behave incorrectly only on a particular date? We'd have to run the
tests on that specific date to trigger it. If we introduce a bug, we
want to know about it *immediately*. Not weeks, months, or even years
after the bug was introduced. Furthermore, if we find such a bug with
our tests, we can't easily reproduce it on another date. We'd have to
rewrite the implementation code to trigger the bug again.

Instead of using `IO`, we'll use a simply technique for keeping our
function pure: take all the information the function needs as arguments.
In the case of `validateSignup`, we'll pass today's date as the first
argument:

``` {.haskell .numbers}
validateSignup
  :: Day -> SignupForm -> Validation (NonEmpty SignupError) Signup
```

Again, let's not worry about the implementation just yet. We'll focus on
the tests.

### Generating Dates

In order to test the new `validateSignup` implementation, we need to
generate `Day` values. We're going to use a few functions from a
separate module called `Data.Time.Gen`, previously written by some
brilliant developer in our team. Let's look at their type
signatures. The implementations are not very interesting.

The generator, `day`, generates a day within the given range:

``` {.haskell .numbers}
day :: Range Day -> Gen Day
```

A day range is constructed with `linearDay`:

``` {.haskell .numbers}
linearDay :: Day -> Day -> Range Day
```

Alternatively, we might use `exponentialDay`:

``` {.haskell .numbers}
exponentialDay :: Day -> Day -> Range Day
```

The `linearDay` and `exponentialDay` range functions are analoguous to
Hedgehog's `linear` and `exponential` ranges for integral numbers.

To use the generator functions from `Data.Time.Gen`, we first add an
import, qualified as `Time`:

``` {.haskell .numbers}
import qualified Data.Time.Gen      as Time
```

Next, we define a generator `anyDay`:

``` {.haskell .numbers}
anyDay :: Gen Day
anyDay =
  let low  = fromGregorian 1900 1 1
      high = fromGregorian 2100 12 31
  in  Time.day (Time.linearDay low high)
```

The date range $[\text{1900-01-01}, \text{2100-12-31}]$ is arbitrary. We
could pick any centuries we like, provided the `time` package supports
the range. But why not make it somewhat realistic?

### Rewriting Existing Properties

Now, it's time to rewrite our existing property tests. Let's begin with
the one testing that validating a form with all valid data succeeds:

``` {.haskell .numbers}
prop_valid_signup_form_succeeds = property $ do
  today <- forAll anyDay ➊
  let genForm = SignupForm <$> validName <*> validBirthDate today
  form <- forAll genForm ➋

  case validateSignup today form of
    Success{}        -> pure ()
    Failure failure' -> do
      annotateShow failure'
      failure
```

A few new things are going on here. We're generating a date representing
today (➊), and generating a form with a birth date based on today's
date (➋). Generating today's date, we're effectively time travelling
and running the form validation on that date. This means our
`validBirthDate` generator must know which date is today, in order to
pick a valid birth date. We pass today's date as a parameter, and
generate a date within the range of 18 to 150 years earlier:

``` {.haskell .numbers}
validBirthDate :: Day -> Gen Day
validBirthDate today = do
  n <- Gen.integral (Range.linear 18 150)
  pure (n `yearsBefore` today)
```

We define the helper function `yearsBefore` in the test suite. It
offsets a date backwards in time by a given number of years:

``` {.haskell .numbers}
yearsBefore :: Integer -> Day -> Day
yearsBefore years = addGregorianYearsClip (negate years)
```

The `Data.Time.Calendar` module exports the `addGregorianYearsClip`
function. It adds a number of years, clipping February 29th (leap days)
to February 28th where necessary.

Let's run tests:

``` {.hedgehog}
λ> check prop_valid_signup_form_succeeds
  ✓ <interactive> passed 100 tests.
```

Let's move on to the next property, checking that invalid birth dates do
*not* pass validation. Here, we use the same pattern as before,
generating today's date, but use `invalidBirthDate` instead:

``` {.haskell .numbers}
prop_invalid_age_fails = property $ do
  today <- forAll anyDay
  form <- forAll (SignupForm <$> validName <*> invalidBirthDate today)

  cover 5 "not yet born" (formBirthDate form > today)
  cover 5 "too young" (formBirthDate form > 18 `yearsBefore` today)
  cover 5 "too old" (formBirthDate form < 150 `yearsBefore` today)

  case validateSignup today form of
    Failure (TooYoung{}   :| []) -> pure ()
    Failure (NotYetBorn{} :| []) -> pure ()
    Failure (TooOld{}     :| []) -> pure ()
    other                        -> do
      annotateShow other
      failure
```

Notice that we've also adjusted the coverage checks. There's a new
label, "not born yet," for birth dates in the future. Running tests, we
see the label in action:

``` {.hedgehog}
λ> check prop_invalid_age_fails
  ✓ <interactive> passed 100 tests.
    not yet born 18% ███▌················ ✓ 5%
    too young    54% ██████████▊········· ✓ 5%
    too old      46% █████████▏·········· ✓ 5%
```

Good coverage, all tests passing. We're not quite done, though. There's
a particular set of dates that we should be sure to cover: "today" dates
and birth dates that are close to, or exactly, 18 years apart.

Within our current property test for invalid ages, we're only sure that
generated birth dates include at least 5% too old, and at least 5% too
young. We don't know how far away from the "18 years" validation
boundary they are.

We could tweak our existing generators to produce values close to that
boundary. Given a date $T$, exactly 18 years before today's date, then:

-   `invalidBirthDate` would need to produce birth dates just after but
    not equal to $T$
-   `validBirthDate` would need to produce birth dates just before or
    equal to $T$

There's another option, though. Instead of defining separate properties
for valid and invalid ages, we'll use a *single* property for all cases.
This way, we only need a single generator.

## A Single Validation Property

In [Building on developers' intuitions to create effective
property-based tests](https://www.youtube.com/watch?v=NcJOiQlzlXQ), John
Hughes talks about "one property to rule them all." Similarly, we'll
define a single property `prop_validates_age` for birth date validation.
We'll base our new property on `prop_invalid_age_fails`, but generalize
to cover both positive and negative tests:

``` {.haskell .numbers}
prop_validates_age = property $ do
  today <- forAll anyDay
  form  <- forAll (SignupForm <$> validName <*> anyBirthDate today) ➊

  let tooYoung        = formBirthDate form > 18 `yearsBefore` today ➋
      notYetBorn      = formBirthDate form > today
      tooOld          = formBirthDate form < 150 `yearsBefore` today
      oldEnough       = formBirthDate form <= 18 `yearsBefore` today
      exactly age = formBirthDate form == age `yearsBefore` today
      closeTo age =
        let diff' =
                diffDays (formBirthDate form) (age `yearsBefore` today)
        in  abs diff' `elem` [0 .. 2]

  cover 10 "too young"    tooYoung
  cover 1  "not yet born" notYetBorn
  cover 1  "too old"      tooOld

  cover 20 "old enough"   oldEnough ➌
  cover 1  "exactly 18"   (exactly 18)
  cover 5  "close to 18"  (closeTo 18)

  case validateSignup today form of ➍
    Failure (NotYetBorn{} :| []) | notYetBorn -> pure ()
    Failure (TooYoung{} :| []) | tooYoung -> pure ()
    Failure (TooOld{} :| []) | tooOld -> pure ()
    Success{} | oldEnough             -> pure ()
    other                             -> annotateShow other >> failure
```

There are a few new things going on here:

1.  Instead of generating exclusively invalid or valid birth dates,
    we're now generating *any* birth date based on today's date
2.  The boolean expressions are used both in coverage checks and in
    asserting, so we separate them in a `let` binding
3.  We add three new labels for the valid cases
4.  Finally, we assert on both valid and invalid cases, based on the
    same expressions used in coverage checks

Note that our assertions are more specific than in
`prop_invalid_age_fails`. The failure cases only pass if the
corresponding label expressions are true. The `oldEnough` case covers
all valid birth dates. Any result other than the four expected cases is
considered incorrect.

The `anyBirthDate` generator is based on today's date:

``` {.haskell .numbers}
anyBirthDate :: Day -> Gen Day
anyBirthDate today =
  let ➊
      inPast range = do
        years <- Gen.integral range
        pure (years `yearsBefore` today)
      inFuture = do
        years <- Gen.integral (Range.linear 1 5)
        pure (addGregorianYearsRollOver years today)
      daysAroundEighteenthYearsAgo = do
        days <- Gen.integral (Range.linearFrom 0 (-2) 2)
        pure (addDays days (18 `yearsBefore` today))
  in  ➋
      Gen.frequency
        [ (5, inPast (Range.exponential 1 150))
        , (1, inPast (Range.exponential 151 200))
        , (2, inFuture)
        , (2, daysAroundEighteenthYearsAgo)
        ]
```

We defines helper functions (➊) for generating dates in the past, in
the future, and close to 18 years ago. Using those helper functions, we
combine four generators, with different date ranges, using a
`Gen.frequency` distribution (➋). The weights we use are selected to
give us a good coverage.

Let's run some tests:

``` {.hedgehog}
λ> check prop_validates_age
  ✓ <interactive> passed 100 tests.
    too young    62% ████████████▍······· ✓ 10%
    not yet born 20% ████················ ✓  1%
    too old       4% ▊··················· ✓  1%
    old enough   38% ███████▌············ ✓ 20%
    exactly 18   16% ███▏················ ✓  1%
    close to 18  21% ████▏··············· ✓  5%
```

Looks good! We've gone from testing positive and negative cases
separately, to instead have a single property covering all cases, based
on a single generator. It's now easier to generate values close to the
valid/invalid boundary of our SUT, i.e. around 18 years from today's
date.

## February 29th

For the fun of it, let's run some more tests. We'll crank it up to
20000.

``` {.hedgehog .numbers}
λ> check (withTests 20000 prop_validates_age)
  ✗ <interactive> failed at test/Validation/V3Test.hs:141:64
    after 17000 tests and 25 shrinks.
    too young    60% ████████████········ ✓ 10%
    not yet born 20% ███▉················ ✓  1%
    too old       9% █▉·················· ✓  1%
    old enough   40% ███████▉············ ✓ 20%
    exactly 18   14% ██▊················· ✓  1%
    close to 18  21% ████▎··············· ✓  5%

        ┏━━ test/Validation/V3Test.hs ━━━
    114 ┃ prop_validates_age = property $ do
    115 ┃   today <- forAll anyDay
        ┃   │ 1956 - 02 - 29
    116 ┃   form  <- forAll (SignupForm <$> validName <*> anyBirthDate today) ➊
        ┃   │ SignupForm { formName = "aa" , formBirthDate = 1938 - 03 - 01 }
    117 ┃
    118 ┃   let tooYoung        = formBirthDate form > 18 `yearsBefore` today ➋
    119 ┃       notYetBorn      = formBirthDate form > today
    120 ┃       tooOld          = formBirthDate form < 150 `yearsBefore` today
    121 ┃       oldEnough       = formBirthDate form <= 18 `yearsBefore` today
    122 ┃       exactlyEighteen = formBirthDate form == 18 `yearsBefore` today
    123 ┃       closeToEighteen =
    124 ┃         let diff' =
    125 ┃                 diffDays (formBirthDate form) (18 `yearsBefore` today)
    126 ┃         in  abs diff' `elem` [0 .. 2]
    127 ┃
    128 ┃   cover 10 "too young"    tooYoung
    129 ┃   cover 1  "not yet born" notYetBorn
    130 ┃   cover 1  "too old"      tooOld
    131 ┃
    132 ┃   cover 20 "old enough"   oldEnough ➌
    133 ┃   cover 1  "exactly 18"   exactlyEighteen
    134 ┃   cover 5  "close to 18"  closeToEighteen
    135 ┃
    136 ┃   case validateSignup today form of ➍
    137 ┃     Failure (NotYetBorn{} :| []) | notYetBorn -> pure ()
    138 ┃     Failure (TooYoung{} :| []) | tooYoung -> pure ()
    139 ┃     Failure (TooOld{} :| []) | tooOld -> pure ()
    140 ┃     Success{} | oldEnough             -> pure ()
    141 ┃     other                             -> annotateShow other >> failure
        ┃     │ Success Signup { name = "aa" , birthDate = 1938 - 03 - 01 }
        ┃     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

Failure! Chaos! What's going on here? Let's examine the test case:

-   Today's date is 1956-02-29
-   The birth date is 1938-03-01
-   The validation function considers this *valid* (it returns a
    `Success` value)
-   The test does considers this *invalid* (`oldEnough` is `False`)

This means that when the validation runs on a leap day, [February
29th](https://en.wikipedia.org/wiki/February_29#Born_on_February_29),
and the person would turn 18 years old the day after (on March 1st), the
validation function incorrectly considers the person old enough. We've
found a bug.

### Test Count and Coverage

Two things led us to find this bug:

1.  Most importantly, that we generate today's date and pass it as a
    parameter. Had we used the actual date, retrieved with an IO action,
    we'd only be able to find this bug every 1461 days. Pure functions
    are easier to test.
2.  That we ran more tests than the default of 100. We might not have
    found this bug until much later, when the generated dates happened
    to trigger this particular bug. In fact, running 20000 tests does
    not always trigger the bug.

Our systems are often too complex to be tested exhaustively. Let's use
our form validation as an example. Between 1900-01-01 and 2100-12-31
there are 73,413 days. Selecting today's date and the birth date from
that range, we have more than five billion combinations. Running that
many Hedgehog tests in GHCi on my laptop (based on some quick
benchmarks) would take about a month. And this is a simple pure
validation function!

To increase coverage, even if it's not going to be exhaustive, we can
increase the number of tests we run. But how many should we run? On a
continuous integration server we might be able to run more than we do
locally, but we still want to keep a tight feedback loop. And what if
our generators never produce inputs that reveal existing bugs,
regardless of the number of tests we run?

If we can't test exhaustively, we need to ensure our generators cover
interesting combinations of inputs. We need to carefully design and
measure our tests and generators, based on the edge cases we already
know of, as well as the ones that we discover over time. PBT without
measuring coverage easily turns into a false sense of security.

In the case of our leap day bug, we can catch it with fewer tests, and
on every test run. We need to make sure we cover leap days, used both as
today's date and as the birth date, even with a low number of tests.

### Covering Leap Days

To generate inputs that cover certain edge cases, we combine specific
generators using `Gen.frequency`:

``` {.haskell .numbers}
(today, birthDate') <- forAll
  (Gen.frequency
    [ (5, anyDayAndBirthDate) ➊

    , (2, anyDayAndBirthDateAroundYearsAgo 18) ➋
    , (2, anyDayAndBirthDateAroundYearsAgo 150)

    , (1, leapDayAndBirthDateAroundYearsAgo 18) ➌
    , (1, leapDayAndBirthDateAroundYearsAgo 150)

    , (1, commonDayAndLeaplingBirthDateAroundYearsAgo 18) ➍
    , (1, commonDayAndLeaplingBirthDateAroundYearsAgo 150)
    ]
  )
```

Arbitrary values for today's date and the birth date are drawn most
frequently (➊), with a weight of 5. Next, with weights of 2, are
generators for cases close to the boundaries of the validation function
(➋). Finally, with weights of 1, are generators for special cases
involving leap days as today's date (➌) and leap days as birth date
(➍).

Note that these generators return pairs of dates. For most of these
generators, there's a strong relation between today's date and the birth
date. For example, we can't first generate *any* today's date, pass that
into a generator function, and expect it to always generate a leap day
that occured 18 years ago. Such a generator would have to first generate
the leap day and then today's date.

Let's define the generators. The first one, `anyDayAndBirthDate`, picks
any today's date within a wide date range. It also picks a birth date
from an even wider date range, resulting in some future birth dates and
some ages above 150.

``` {.haskell .numbers}
anyDayAndBirthDate :: Gen (Day, Day)
anyDayAndBirthDate = do
  today <- Time.day
    (Time.linearDay (fromGregorian 1900 1 1)
                    (fromGregorian 2020 12 31)
    )
  birthDate' <- Time.day
    (Time.linearDay (fromGregorian 1850 1 1)
                    (fromGregorian 2050 12 31)
    )
  pure (today, birthDate')
```

Writing automated tests with a hard-coded year 2020 might scare you.
Won't these tests fail when run in the future? No, not these tests.
Remember, the validation function is deterministic. We control today's
date. The *actual* date on which we run these tests doesn't matter.

Similar to the previous generator is
`anyDayAndBirthDateAroundYearsAgo`.  First, it generates any date as
today's date (➊). Next, it generates an arbitrary date approximately
some number of years ago (➋), where the number of years is an
argument of the generator.

``` {.haskell .numbers}
anyDayAndBirthDateAroundYearsAgo :: Integer -> Gen (Day, Day)
anyDayAndBirthDateAroundYearsAgo years = do
  today <- Time.day ➊
    (Time.linearDay (fromGregorian 1900 1 1)
                    (fromGregorian 2020 12 31)
    )
  birthDate' <- addingApproxYears (negate years) today ➋
  pure (today, birthDate')
```

The `addingApproxYearsAgo` generator adds a number of years to a date,
and offsets it between two days back and two days forward in time.

``` {.haskell .numbers}
addingApproxYears :: Integer -> Day -> Gen Day
addingApproxYears years today = do
  days <- Gen.integral (Range.linearFrom 0 (-2) 2)
  pure (addDays days (addGregorianYearsRollOver years today))
```

The last two generators used in our `frequency` distribution cover leap
day edge cases. First, let's define the
`leapDayAndBirthDateAroundYearsAgo` generator. It generates a leap day
used as today's date, and a birth date close to the given number of
years ago.

``` {.haskell .numbers}
leapDayAndBirthDateAroundYearsAgo :: Integer -> Gen (Day, Day)
leapDayAndBirthDateAroundYearsAgo years = do
  today      <- leapDay (Range.linear 1904 2020)
  birthDate' <- addingApproxYears (negate years) today
  pure (today, birthDate')
```

The `leapDay` generator uses `mod` to only generate years divisible by 4
and constructs dates on February 29th. That alone isn't enough to only
generate valid leap days, though. Years divisible by 100 but not by 400
are not leap years. To keep the generator simple, we discard those years
using the already existing `isLeapDay` predicate as a filter.

``` {.haskell .numbers}
leapDay :: Range Integer -> Gen Day
leapDay yearRange = Gen.filter isLeapDay $ do
  year <- Gen.integral yearRange
  pure (fromGregorian (year - year `mod` 4) 2 29)
```

In general, we should be careful about discarding generated values using
`filter`. If we discard too much, Hedgehog gives up and complains
loudly. In this particular case, discarding a few generated dates is
fine. Depending on the year range we pass it, we might not discard any
date.

Finally, we define the `commonDayAndLeaplingBirthDateAroundYearsAgo`
generator. It first generates a leap day used as the birth date, and
then a today's date approximately the given number of years after the
birth date.

``` {.haskell .numbers}
commonDayAndLeaplingBirthDateAroundYearsAgo :: Integer -> Gen (Day, Day)
commonDayAndLeaplingBirthDateAroundYearsAgo years = do
  birthDate' <- leapDay (Range.linear 1904 2020)
  today <- addingApproxYears years birthDate'
  pure (today, birthDate')
```

That's it for the generators. Now, how do we know that we're covering
the edge cases well enough? With coverage checks!

``` {.haskell .numbers}

cover 5 ➊
      "close to 18, validated on common day"
      (closeTo 18 && not (isLeapDay today))
cover 1
      "close to 18, validated on leap day"
      (closeTo 18 && isLeapDay today)

cover 5 ➋
      "close to 150, validated on common day"
      (closeTo 150 && not (isLeapDay today))
cover 1
      "close to 150, validated on leap day"
      (closeTo 150 && isLeapDay today)

cover 5 ➌
      "exactly 18 today, born on common day"
      (exactly 18 && not (isLeapDay birthDate'))
cover ➍
  1
  "legally 18 today, born on leap day"
  (  isLeapDay birthDate'
  && (addGregorianYearsRollOver 18 birthDate' == today)
  )
```

We add new checks to the property test, checking that we hit both leap
day and regular day cases around the 18th birthday (➊) and the 150th
birthday (➋). Notice that we had similar checks before, but we were
not discriminating between leap days and common days.

Finally, we check the coverage of two leap day scenarios that can occur
when a person [legally turns
18](https://en.wikipedia.org/wiki/February_29#Legal_status): a person
born on a common day turning 18 on a leap day (➌), and a leapling
turning 18 on a common day (➍).

Running the modified property test, we get the leap day counter-example
every time, even with as few as a hundred tests. For example, we might
see today's date being 1904-02-29 and the birth date being 1886-03-01.
The validation function deems the person old enough. Again, this is
incorrect.

Now that we can quickly and reliably reproduce the failing example we
are in a great position to find the error. While we could use a fixed
seed to reproduce the particular failing case from the 20000 tests run,
we are now more confident that the property test would catch future leap
day-related bugs, if we were to introduce new ones. Digging into the
implementation, we'll find a boolean expression in a pattern guard being
the culprit:

``` {.haskell .numbers}
birthDate' <= addGregorianYearsRollOver (-18) today
```

The use of `addGregorianYearsRollOver` together with adding a negative
number of years is the problem, rolling over to March 1st instead of
clipping to February 28th. Instead, we should use
`addGregorianYearsClip`:

``` {.haskell .numbers}
birthDate' <= addGregorianYearsClip (-18) today
```

Running 100 tests again, we see that they all pass, and that our
coverage requirements are met.

``` {.hedgehog}
λ> Hedgehog.check prop_validates_age
  ✓ <interactive> passed 100 tests.
    too young                             17% ███▍················ ✓ 10%
    not yet born                           7% █▍·················· ✓  1%
    too old                               19% ███▊················ ✓  1%
    old enough                            83% ████████████████▌··· ✓ 20%
    close to 18, validated on common day  30% ██████·············· ✓  5%
    close to 18, validated on leap day     2% ▍··················· ✓  1%
    close to 150, validated on common day 31% ██████▏············· ✓  5%
    close to 150, validated on leap day    6% █▏·················· ✓  1%
    exactly 18 today, born on common day  17% ███▍················ ✓  5%
    legally 18 today, born on leap day     5% █··················· ✓  1%
```

## Summary

In this tutorial, we started with a simple form validation function,
checking the name and age of a person signing up for an online
service.  We defined property tests for positive and negative tests,
learned how to test generators with coverage checks, and found bugs in
both the test suite and the implementation.

When requirements changed, we had to start working with dates. In order
to keep the validation function deterministic, we had to pass in today's
date. This enabled us to simulate the validation running on any date, in
combination with any reported birth date, and trigger bugs that could
otherwise take years to find, if ever. Had we not made it deterministic,
we would likely not have found the leap day bug later on.

To generate inputs that sufficiently test the validation function's
boundaries, we rewrote our separate positive and negative properties
into a single property, and used coverage checks to ensure the quality
of our generators. The trade-off between multiple disjoint properties
and a single more complicated property is hard.

With multiple properties, for example split between positive and
negative tests, both generators and assertions can be simpler and more
targeted. On the other hand, you run a risk of missing certain inputs.
The set of properties might not cover the entire space of inputs.
Furthermore, performing coverage checks across multiple properties,
using multiple targeted generators, can be problematic.

Ensuring coverage of generators in a single property is easier. You
might even get away with a naive generator, depending on the system
you're testing. If not, you'll need to combine more targeted generators,
for example with weighted probabilities. The drawback of using a single
property is that the assertion not only becomes more complicated, it's
also likely to mirror the implementation of the SUT. As we saw with our
single property testing the validation function, the assertion
duplicated the validation rules. You might be able to reuse the coverage
expressions in assertions, but still, there's a strong coupling.

The choice between single or multiple properties comes down to *how* you
want to cover the boundaries of the SUT. Ultimately, both approaches can
achieve the same coverage, in different ways. They both suffer from the
classic problem of a test suite mirroring the system it's testing.

Finally, running a larger number of tests, we found a bug related to
leap days. Again, without having made the validation function
deterministic, this could've only been found on a leap day. We further
refined our generators to cover leap day cases, and found the bug
reliably with as few as 100 tests. The bug was easy to find and fix when
we had the inputs pointing directly towards it.

That's it for this tutorial. Thanks for reading, and happy property
testing and time travelling!
