Thank you for your comments!

We begin with brief responses to the most significant concerns, followed by a
(long and optional) appendix addressing individual questions.

                                -------------

     - How do we determine what a "correct" lens is? Can we make this process a 
       bit more automatic and objective?
       
Determining what a correct lens is quite a difficult problem. What a "correct"
lens is inherently subjective -- determining which components of one format
should be synchronized to another requires knowledge of the formats. If there
were a example large suite of synchronized files of these formats, we could use
this example suite as a ground truth. Unfortunately, such a suite does not
exist, and we then must take it upon ourselves to recognize which components of
the file should be synchronized to which, and validate that the lenses
corresponded to our internal notion of correctness.

In validating that the synthesized lenses were correct, we applied the types of
approaches we use in validating that our code is correct in everyday
development.
1.  We manually inspected the code. This is certainly a nontrivial task, as
    these generated lenses are quite large. However, this task is easier than it
    may initially sound. Much of lenses maintain a rigid structure:
    Concatenations in the lenses typically follow concatenations in the regular
    expressions, disjunctions in the lenses typically follow disjunctions in the
    regular expressions. The primary difficulties arise when seeing how the more
    complex combinators interact. Are the swaps in the right place? Are the
    correct components disconnected? Are lenses merged in the correct way? While
    validation certainly still requires work, instead of closely inspecting
    hundreds of lines of code, we instead must closely inspect a few dozen, and
    give a more cursory glance to the others.
2.  We built unit tests to validate the more complex lenses. Much like everyday
    development, manual inspection of code is rarely sufficient for determining
    correctness. In addition to reading through the code, we added in unit tests
    to validate that the generated code performs correctly on representative
    examples.
3.  Lastly, in addition to unit tests, we ran the lenses on a number of strings,
    taken from the unit tests and the examples, but with minor alterations.
    These quick tests we did not add as unit tests, but served as additional
    validation for ourselves as we were constructing the benchmark suite.

We recognize we did not go into significant enough detail on this in the
submitted version, and will gladly add in such detail on validation in the final
version.

                                -------------

     - Is our synthesis procedure overfit to our benchmarks? Why does assigning
       a fixed probability work?

Our 39 bijective benchmarks came from the original "Synthesizing Bijective
Lenses" benchmark suite without alteration to the regular expressions. Our
additional examples, in particular the data cleaning tasks, were taken from the
original FlashFill paper. Unfortunately, there is no public release of the
FlashFill benchmarks, so our examples had to be taken from the paper, instead of
from a larger benchmark suite.

Assigning a fixed probability is certainly less desirable than inferring it from
data. With a learned distribution, we expect our algorithm to perform better: it
can recognize that certain disjunctions both have high probability, and would
strongly incentivize aligning those critical disjuncts to the critical disjuncts
in the other format. With a learned distribution, we expect that tasks that
currently require 2-3 examples would instead require 1-2. However, this is not
to say that our tool will perform badly as-is on distributions that are far from
our computed ones. On such distributions, our tool will merely not get the
benefits provided by such distributions. It would get an advantage in having an
idea beforehand which components should map to which with a learned
distribution, but merely loses on that advantage without it.

(* This paragraph is to respond to a complaint only of R3, maybe should move
down to below the fold *) 

This is not to say that SREs and our semantic metric are not critical for
success. While we use a learned distribution, using such a semantic metric
provides a means of comparison across proposed pairs. With a syntactic metric,
we aren't making an "apples-to-apples" comparison, one lens might have a higher
score, but that score would be based on a different distribution of data - it
actually would be less likely with a different data distribution. Keeping the
probabilities of the two formats is critical for comparing between proposed
types.


===========================================================================
===========================================================================
Detailed responses to individual questions in the reviews follow...


===========================================================================
Review #43A

                                -------------

     - I have no significant complaints about this paper apart from Section 3 
       not making it clear that stochastic regular expressions are not a
       contribution of the authors (albeit it is mentioned in passing on p2).

We did not intend to forget such citations or claim that we are introducing
stochastic regular expressions. We will be sure, in future versions, to make
this distinction more clear.

                                -------------

     - 3. p5 l236,
     
          Shouldn't the second argument to `disconnect` be `company`?

We should have the second type be "" (to be consistent with what we use in the
final lens)

                                -------------

     - 4. p6, l288,
       
          If I understood correctly, this parituclar lens `disconnect(...)`
          picks the default values at random, but is still valid because it
          satisfies the required lens type. If so, please make it clearer.

This is correct

                                -------------

     - 9. p16, l777,

          Could you give concrete examples of situations where all sequences
          are involved in sequence lenses, but there are examples that could
          make other lenses more useful?

Yes, one of our benchmarks is such an example. In particular, consider trying to
synchronize Windows "Scheduled Tasks" and Linux "cron jobs." In such a task, we
wish to synchronize the times that jobs are run, but not synchronize the
commands to run the jobs themselves. In such a scenario, we could create a
bijective lens, but this is not correct behavior. Instead, examples can
demonstrate that these components should remain unsynchronized.

                                -------------

     - 10. p18, l842
       
           Could you explain in some more detail what criteria does `EXPAND` use
           to decide to open some closed expressions but not others?

Full details are present in the prior work on Synthesizing Bijective Lenses
(page 16). From a high level perspective, we process both regular expression
trees, looking for situations where a regular expression is as a subcomponent of
one format, but not the other, and expand such REs (this is where hashconsing
provides its greatest benefits). Furthermore, we look for instances where a
regular expressions may be present in one format, but is only contained within a
closed regular expressions in the other, and expand the closed RE containing the
missing RE. We then make this apply in slightly more places by also taking into
account how deeply nested within stars certain REs occur.

                                -------------

     - 11. p19, l887
       
           Surely the definition of `CONTINUE(pq)` should be using `max`? Otherwise
           I don't see how the definition makes sense, because using surely `min` 
           will just pick 0 for t>=0, because the distance and the logarithm are
           positive values.

Correct, this should be max

                                -------------

     - 12. p21, l1022
               
           Which preference metric are the t=0, t=25, t=-25 benchmarks using?

These benchmarks are using the full "SS" mode. The only difference between these
benchmarks and the full mode is that termination parameters are ignored, and
are instead required to use the 0, 25, and -25 parameters.

                                -------------

      - Second, from what I remember from the literature on (ordinary) symmetric
        lenses, they do not lend themselves very well to analysis in terms of 
        spans of asymmetric lenses. With simple symmetric lenses, do you think
        the situation is better now that you have omitted the complement part?

TODO: Learn Category Theory

                                -------------

       - Thirdly, do the authors expect to be able to extend the proposed 
         synthesis algorithm to handle any of the many variants of updates-based
         lenses, such as edit lenses, delta lenses, and update lenses?

Yes, I do expect that the algorithm can be extended to handle such variants,
though likely it would require some additional work.

Delta lenses would require additional work in understanding "shape" constructs.
Currently our language only permits basic iteration, but shapes allow for
different types of mergings. This would require changes to AtomSynth, in that we
would we would need to discover the correct shapes of the data, and align them
in the correct ways.

Both edit and update lenses would require new notions of specifications. Both
lens types permit updates that are not merely pushing data through, but instead
are applying specific elements of an edit monoid. We expect that our current IO
approach to specifications would not work as we wished for such transformations.
(Also we would remove complements from edit lenses).



===========================================================================
Review #43B

                                -------------

     - The main weakness is in the evaluation part. There are many 
       unclear/controversial parts regarding the evaluation. For example, the 
       lack of description of the benchmark environment; the way they test 
       whether the synthesised lens is correct; the description of the tasks to
       be synthesised. See detailed comments below. (This defect does not
       prevent it from being a good paper.)

All benchmarks were performed on a 2.5 GHz Intel Core i7 processor with 16 GB of
1600 MHz DDR3 running macOS High Sierra. We provide a more detailed answer our
benchmark suite and method of finding correctness in the main portion of the
paper.

                                -------------

     - The first subsection of Related Work did not give any reference to
       the related work except for Foster et al. 2007, which makes the
       paper unfriendly and hard to verify. The appendix is also in a
       messy.

We apologize for the lack of citations and messy appendix. For the first
subsection of Related Work, we got carried away in making formal claims, and did
not include information comparisons to other lens languages. We will make sure
to make such comparisons and citations in the final version of the paper. We
will similarly improve the appendix.

                                -------------

     - L11: The reference Martin Hofmann et al. (2015) does not help since it
       is not yet published and cannot be obtained from Internet. Martin
       Hofmann et al. (2011) may be the choice for this time.

Thank you, you are correct, we will change this citation.

                                -------------

     - L442: H(s) should be 0. Otherwise the entropy of the SRE in L452 is 2.

You are correct.

                                -------------

     - L484 and L485:
       I cannot see the reason to relax the restriction regarding unambiguous
       REs. What will happen if primitive lens combinators for unambiguous
       REs are composed with the ones for ambiguous REs? Since complex lenses
       are composed from (ambiguous and unambiguous) primitive lenses, in
       practice the restriction should be always there.

Within our synthesis algorithm, you are, in general, correct. However, Boomerang
exists beyond Optician, and permits lenses like id("a"|"a"). In our extension to
Boomerang, we do not remove such freedom, and continue to permit such languages.

                                -------------

     - L557 and below: Why some of the entropy is a single value while some
       of them seems ranging over an interval? If the interval is used to
       handle `merge_left`, then the entropy of all combinators should be an
       interval. Currently only `id`, `disconnect`, and `merge_left` yield
       intervals.

This was us trying to make our equations more readable, but we did not
sufficiently explain what our syntax meant. The calls to H(...) return pairs,
and when multiplied by constants, they act as follows:

a x (b,c) = (a x b,a x c)
(a,b) + (c,d) = (a + b,c + d)

We will make sure to explain this more clearly in a final version of the paper.

                                -------------

     - L718 and L719: Different kinds of data definitely have different
       shapes. I cannot understand why assigning a fixed probability will fit
       in all situations. Could you explain it a bit?

See the main response above the fold.

                                -------------
                             
     - L866 - L868: The user provides two REs describing the source and view
       as input, which should already give some useful structure. For
       example, the top-level nonterminal may be described by several other
       non-terminals. This structure indeed suggests that a big problem can
       be divided into several subproblems.  Why do the user still need to
       manually "generate subproblems"? Is it avoidable?

We do not believe such subproblems are straightforwardly avoidable. While we can
attempt to match up subcomponents, this matching oftentimes will not work
because of, for example, issues where various equivalences must be applied (for
example: ("a"|"b")|("c"|"d") <-> "a"|("b"|("c"|"d"))) do not break down. If we
had attempted to initially try breaking down the problem syntactically, we would
have wasted time. This is particularly an issue because bad specifications could
take a very long time, as there may be no good lenses between the attempted
proposed subcomponents, so there would potentially be a great deal of wasted
time in these attempts.

                                -------------
                             
     - L900: Before introducing the benchmark suite, the authors had better
       provide the specification, regarding CPU, memory, etc., of their
       benchmark environment since an important measurement is the speed of
       the synthesis process.

All benchmarks were performed on a 2.5 GHz Intel Core i7 processor with 16 GB of
1600 MHz DDR3 running macOS High Sierra.

                                -------------
                             
     - L909 and L956: About the benchmark suite, how many of the benchmarks
       in fact only need bijective lenses? This definitely should be
       considered since (1) the benchmark suite is adapted from the "original
       Optician benchmarks, comprised of 39 bijective synthesis tasks", and
       (2) "Adjusting the termination parameter was needed in only 9 of the
       48 tasks". I suppose it is not, but still wonder, if for all of the 48
       - 39 = 9 non bijective tasks, we need to adjust their termination
       parameters?

35 of the benchmarks only need bijective lenses (4 of the original Optician
benchmarks had to be altered to be bijective because of projected data, in the
symmetric benchmark suite we removed such alterations.) Of the 9 benchmarks that
required the termination parameter, 4 of them required only bijective lenses.

                                -------------
                             
     - L922: The authors may add a few words on how representative and
       complex the selected tasks is. For example, I got the data from the
       paper *Deep API Programmer: Learning to Program with APIs* that there
       is 238 real-world FlashFill benchmarks; then why the 8 data cleaning
       tasks here are more representative among the 238 tasks? The reviewers
       (and readers in the future) may want to know to which extent the job
       of writing lenses can be automatically done.

We chose our benchmarks from the FlashFill paper, because the 238 tasks are,
unfortunately, not publicly available. In general, our benchmark suite goes
beyond the level of difficulty required for data cleaning. Many real-life file
format descriptors involve large amounts of disjunctions and nested iterations,
which FlashFill is either bad at (for disjunctions) or cannot do (nested
iterations).

A more thorough comparison to FlashFill is present in the previous Synthesizing
Bijective Lenses paper.

                                -------------
                             
     - L926 and L952: This is the most confusing point for me. The meaning of
       "correct lenses" here seems very subjective as the authors let the
       user determine whether the synthesised lens is desired. Can we make
       this process a bit automatic and objective?

       For instance, usually a natural and reasonable approach is to divide
       examples into a "training" set and a "validation" set; we use the
       training set to synthesise desired lenses and use the validation set
       to test if the synthesised lenses work well (although this does not
       prove its correctness). Even if the user is finally involved in
       determining whether the lens is a desired one, this process may reduce
       the burden of the user.
       
       (I also wonder how can the user verifies whether a complex lens of
       hundreds or thousands of lines code is the desired one by directly
       inspecting the lens program?)

We respond to this in detail in the main response.

                                -------------
                             
     - L945 and L993 (Fig.6 and Fig.7): Since the time is only within 30
       seconds, why not make Figures whose x-axes are linear? With the
       "non-linear figures", I cannot tell that SS took 1.3 times longer than
       BS. In addition, since we only care about the average time of tasks
       (and the maximum time of a single task), there is no reason to use a
       line chart, whose shape is drastically affected by the order of
       tasks. A simple table is better, if the details of tasks (and their
       order) are not given.

We chose a logarithmic x axis to show the differences on the fast benchmarks, as
they otherwise would be imperceptable. However, if the impact on comparing the
slower benchmarks is too significant, we can change to a linear x axis.

Our graph does not include any specific order of tasks. At any given time, a
reader can see how many of the benchmarks are able to complete within that time
(for example, 41 benchmarks require complete in under a second in the full
synthesis algorithm). We can reword the description of these graphs to make this
more clear.

                                -------------
                             
     - L1066: When discussing symmetric lenses, the authors need to give a
       reference to Symmetric Lenses by Martin Hofmann et al. (2011) since
       the definitions and notations are from there.

       L1079: The authors silently move to introduce edit lenses, although
       different from Edit Lenses by Martin Hofmann et al. (2012), a short
       introduction and a reference are needed. Moreover, the authors may
       need to explicitly mention that the edits in this section are
       "overwrites"; otherwise it is difficult to understand why the edits
       have the same type as the source and view (ignoring the sum types made
       by inl and inr.), as usually they are some "deltas".

We will update the lens section of the related work to be more thorough on the
related work that we do not make formal statements about, and include proper
citations.
found in the appendix.

                                -------------

     - L1111 and L1112: I cannot figure out where does `x` below the
       "horizontal line" come from

This is true for any arbitrary "x"



===========================================================================
Review #43C

                                -------------

    - The usability aspect is questionable: It seems that if the user has 
      defined complex regular expressions defining the pair of languages, then
      they can probably go ahead and just implement the lens without resorting 
      to examples.
      
As a demonstration about the difficulty of writing lenses, consider the
following bidirectional program
      
let single_author_convert =
  ins " au - "
    . lens_swap
        (NAME . del ",")
        (lens_swap WSP NAME)*
in

Even the relatively simple lens that permutes the 3 elements needed for
single_author_convert is fairly complex.  The complexity becomes even more
apparent when working with large lenses comprised of many sub-lenses (though
for brevity we give here the more minimal example above).  Lenses provide
great power with their invertibility guarantees, but they come at the cost
of thinking about many fiddly details when writing the terms.  Worrying
about these fiddly details while ALSO thinking about unambiguity
restrictions is a very difficult task!

Engineering best practices dictate that we annotate the lenses with their types.
While Boomerang does not require these annotations - being able to infer the
types from the terms - the types of the term serve as documentation for future
programmers to understand what formats the lens maps between. Furthermore, the
types provide resilience in the face of future lens modifications, as the types
ensure that the lens maps exactly between strings of the provided formats.
Invalid inputs are not accepted nor are the outputs ever invalid. Consequently,
in a well-maintained codebase, we are not requiring additional work. Instead, we
are saving programmers from having to writing the lens by leveraging work they
should already be performing.

                                -------------
                                
    - More important: It is unclear to the general reader why the Expand
      procedure is needed — i.e., why do we need to rewrite types for
      GreedySynth.

We will make this more clear in future versions of the paper. Expand is needed
because GreedySynth alone cannot traverse regular expression equivalences. If
given the two regular expressions "" | "a"+, and "a"\*, GreedySynth alone would
merge "" to "a"\* (with a disconect), and merge "a"+ (also with a disconect).
However, contuing search through equivalent REs helps find the bijective lens.

                                -------------
                                
    - The ToStochastic procedure felt quite ad hoc to me. The paper makes a 
      point that if the user knew the distribution (e.g., if they automatically
      infer the parameters from data), then they can supply it. But will that
      really help the search? Essentially, I wish there were examples (even 
      synthetic ones) where knowing the distribution of data can clearly 
      influence the synthesis procedure.

Consider the situation where the input is two regular expressions:
"a"+ | "b"+ <=> "c" | "d"

There are two lenses that are both require projecting the same iteration that
satisfies this spec, "a"+ can go to "c", or "a"+ can go to "d" (and "b" goes to
the other).

However, if both "a"+ and "c" are high probability, then they would become
aligned first, as it is more important they get low cost lenses, as they occur
more often. This is desired, as in synchronized formats, we expect synchronized
disjuncts to have similar probabilities.

                                -------------
                                
    - The paper resorts to a fixed 
      distribution that gives the same probability to each disjunct. This raises
      two concerns:

      1.  that this overfits to the hand-picked benchmarks and 
      2.  that the entropy calculation and stochastic REs are an overkill. 

      Why not use a syntactic heuristic. That’s what DC is trying to do, but 
      perhaps a slightly more sophisticated syntactic heuristic could achieve 
      the same effect. So, while the use of entropy is cool, appealing, and new 
      in synthesis (to my knowledge), the paper does not convince me that it is 
      needed here.

We respond to these issues in the main reponse.

                                -------------
                                
    - Section 6 or 7 should discuss examples that are outside the expressivity 
      of the lens language and/or take too much time. It was hard for me to 
      infer what the limitations of the approach are.

We agree that section 7 requires more, informal comparisons to other lenses, for
us to discuss comparisons between our framework and other types of lenses. In
general, for different types of lenses to work, we require additional work,
either in the synthesis algorithm (for different types of combinators like
matching lenses or delta lenses), or in both the synthesis algorithm and
specifications (like for edit lenses and update lenses).

                                -------------

    - My main concern is whether the use of stochastic REs is really useful (see
      above)
      
Stochastic REs, and more generally, a semantic cost metric is critical for
comparing between responses by GreedySynth.

                                -------------
                                
    - Are stochastic REs new? I didn't see any citations when they were 
      introduced.

Stochastic REs are not new. We cite them when we first mention them on page 2,
but agree we should provide additional citations when they are formally
introduced. We do not mean to imply we are introducing stochastic regular
expressions, though we are introducing syntactic means to find their entropy.

