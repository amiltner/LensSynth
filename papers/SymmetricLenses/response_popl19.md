Thank you for your comments!

We begin with brief responses to the most significant concerns, followed by
an (optional) appendix addressing individual questions.

                                -------------

     - How do we determine whether a lens is "correct"? Can we make this process
       more automatic and objective?
       
We agree we should discuss this point in more detail; we will do so in the final
version.

We do not have an automated way to measure the correctness of our
lenses beyond confirming that they conform to their specifications. In
this respect, our work is similar to other published
synthesis-from-examples research, like FlashFill and related projects.

To validate the synthesized lenses, we did what programmers usually do:

1.  We manually inspected the code. 

    This task was nontrivial, as generated lenses can be quite large, but it
    was easier than it might sound. Some of the lenses' structure should follow
    the structure of their regular expression types; this is relatively easy to
    validate. The more difficult part was checking the interactions of complex
    combinators like swaps, disconnects, and merges. However, these bits
    typically span only a few dozen lines of code. Certainly, checking the
    code is easier than creating it de novo.

2.  We ran a series of unit tests to validate the more complex lenses. These
    tests confirmed that the output of creates and puts were as expected.
    
After manually validating that the SS mode found the desired lenses,
we validated other modes by matching their output to that of SS.


                                -------------

      - A number of related questions about our information-
        theoretic measure: Is it useful? Why not use a 
        (perhaps complex) syntactic metric? 
        Why does a fixed distribution work?
        Will knowing the data distribution help?

It is impossible to prove that our information-theoretic measure is better than
all possible "syntactic" metrics; our experiments show that it performs
well, outperforming simple ones. More importantly, our heuristic is based on a
simple, clear idea that is easy to communicate and implement. This idea can
potentially be reused in a variety of related settings (and perhaps _augmented_
with domain-specific heuristics).

Moreover, our information-theoretic presentation helped us understand some of
the ad hoc metrics used in the past -- like in FlashFill and Refazer -- in which
"less bijective" combinators are assigned higher costs in simpler ways (like
avoiding constants). These ad hoc syntactic metrics seem to be approximating
something like the information-theoretic measure we work with.

A fixed distribution works because the core goal is to generate "more
bijective" transformations; information should be preserved, wherever
possible, when translating data between formats. A fixed, uniform
distribution avoids weighting one conjunct or disjunct over
another. When not given a distribution, this kind of "fairness" seems
to be the best we can do.

If given the distribution, the system can make a more informed choice about what
to throw away. Let (x?.9) mean "x appears with probability .9 and is ""
otherwise." Consider a conversion between these formats:

    (x?.999).(y?.001) <==> z

Here, x appears more frequently than y on the left. The system will
prefer a lens that projects y and maps x to z over a lens that
projects x and maps y to z because the latter throws away more
information on average and is "less bijective" in an
information-theoretic sense.

===========================================================================
===========================================================================
End of main response.

Detailed responses to individual questions in the reviews follow...

===========================================================================
Review #43A

                                -------------

     - I have no significant complaints about this paper apart from Section 3 
       not making it clear that stochastic regular expressions are not a
       contribution of the authors (albeit it is mentioned in passing on p2).

Yes, we will make this clear on p. 3 too.

                                -------------

     - 3. p5 l236,
     
          Shouldn't the second argument to `disconnect` be `company`?

Correct. However, in hindsight, we should change the example so that the
second type is "" to be consistent with the full lens in Figure 3.

                                -------------

     - 4. p6, l288,
       
          If I understood correctly, this parituclar lens `disconnect(...)`
          picks the default values at random, but is still valid because it
          satisfies the required lens type. If so, please make it clearer.

You are correct. We will clarify.

                                -------------

     - 9. p16, l777,

          Could you give concrete examples of situations where all sequences
          are involved in sequence lenses, but there are examples that could
          make other lenses more useful?

Within a DNF lens, all sequences are *required* to pair with another
sequence lens. Otherwise, when given one side of the DNF SRE, what
disjunct on the other side should it be mapped to?  There are other
ways the disjunct can be mapped, but they must happen at a different
level of the synthesis algorithm. For example, the entirety of that
DNF regular expression (which itself could be deeply nested within
other DNF regular expression components) could be
projected. Furthermore, a specific sequence could be mapped to
another, but all the components of that sequence lens may be
disconnects. However, all sequences *must* be involved in a sequence
lens in a well-typed DNF lens.

                                -------------

     - 10. p18, l842
       
           Could you explain in some more detail what criteria does `EXPAND` use
           to decide to open some closed expressions but not others?

EXPAND processes both regular expression trees, looking for situations
where a closed regular expression is a subcomponent of one format but
not the other.  We expand such REs (hash-consing provides great
benefits here). As the internals of closed REs are not exposed to
GreedySynth, this expansion is necessary to synchronize the closed RE
with anything but disconnect. EXPAND also looks for instances where a
regular expression is present in one format but is only contained
within a closed regular expression in the other.  In this case, we
expand the closed RE containing the missing RE.  We apply this
rewriting in a few more places by taking into account how deeply
nested within stars certain REs occur.

Full details can be found in the prior work on Synthesizing Bijective
Lenses (page 16), though in that context, the term "user-defined data
types" is used instead of "closed regular expressions."

                                -------------

     - 11. p19, l887
       
           Surely the definition of `CONTINUE(pq)` should be using `max`? 
           Otherwise I don't see how the definition makes sense, because using 
           surely `min` will just pick 0 for t>=0, because the distance and the
           logarithm are positive values.

Right!

                                -------------

     - 12. p21, l1022
               
           Which preference metric are the t=0, t=25, t=-25 benchmarks using?

These benchmarks use the full "SS" mode. The only difference between these t=0,
t=25, and t=-25 benchmarks and the full "SS" mode is that they ignore
termination parameters and use the 0, 25, and -25 parameters instead.

                                -------------

      - Second, from what I remember from the literature on (ordinary) symmetric
        lenses, they do not lend themselves very well to analysis in terms of 
        spans of asymmetric lenses. With simple symmetric lenses, do you think
        the situation is better now that you have omitted the complement part?

It may be slightly better, but the details are tricky. From "Spans of Delta
Lenses," p1, equivalence relations are needed in both the span-presentation
and set-based presentation of symmetric lenses to address "'hidden' details
such as their *complements*." Simple symmetric lenses intentionally have no
such details. However, there seem to be other complexities related to lens
laws, requiring restrictions like very well-behavedness, as (from "Spans of
Lenses", p1) "in the category whose morphisms are lenses, what one might
expect to be the pullback of lenses need not satisfy the universal property
of a pullback." We do not believe our restriction helps obviate these
difficulties.

                                -------------

       - Thirdly, do the authors expect to be able to extend the proposed 
         synthesis algorithm to handle any of the many variants of updates-based
         lenses, such as edit lenses, delta lenses, and update lenses?

Yes.

Delta lenses would require additional work in understanding "shape"
constructs.  Currently our language only permits basic iteration,
whereas shapes allow for different types of mergings. This extension
would require changes to the AtomSynth algorithm in that we would need
to discover the correct shapes of the data and align them correctly.

Both edit and update lenses would require new forms of specifications,
since both permit updates that are not merely pushing data through,
but instead are applying elements of an edit monoid.  (Also we would
remove complements from edit lenses).

===========================================================================
Review #43B

     - The main weakness is in the evaluation part. There are many 
       unclear/controversial parts regarding the evaluation. For example, the 
       lack of description of the benchmark environment; the way they test 
       whether the synthesised lens is correct; the description of the tasks to
       be synthesised. See detailed comments below. (This defect does not
       prevent it from being a good paper.)

We responded to issues on judging correctness at the top of this response.

We chose our benchmarks from the FlashFill paper because the fuller
set of 238 tasks are not publicly available.  

Many real-life file format descriptors involve large numbers of
disjunctions and nested iterations. FlashFill struggles with
disjunctions and does not handle nested iterations. In our previous
work on synthesizing bijective lenses, we compared FlashFill to our
bijective synthesis algorithm in more detail, finding that FlashFill
was able to synthesize just 5 of the 39 examples that we had at that
time because of nested iterations.  In future version of this paper,
we would be happy to compare our work with FlashFill in greater
detail.

All benchmarks were performed on a 2.5 GHz Intel Core i7 processor with 16
GB of 1600 MHz DDR3 running macOS High Sierra. 
    
                                -------------

     - The first subsection of Related Work did not give any reference to
       the related work except for Foster et al. 2007, which makes the
       paper unfriendly and hard to verify. The appendix is also in a
       messy.

We apologize for the lack of citations and the messy appendix. For the
first subsection of Related Work, we focused on making formal claims,
and did not include informal comparisons to other lens languages. We
will make sure to make such comparisons and include a fuller set of
citations in the next version of the paper. We will similarly improve
the appendix.

                                -------------

     - L11: The reference Martin Hofmann et al. (2015) does not help since it
       is not yet published and cannot be obtained from Internet. Martin
       Hofmann et al. (2011) may be the choice for this time.

Thank you!  We will change this citation.

                                -------------

     - L442: H(s) should be 0. Otherwise the entropy of the SRE in L452 is 2.

You are correct; we will fix it.

                                -------------

     - L484 and L485:
       I cannot see the reason to relax the restriction regarding unambiguous
       REs. What will happen if primitive lens combinators for unambiguous
       REs are composed with the ones for ambiguous REs? Since complex lenses
       are composed from (ambiguous and unambiguous) primitive lenses, in
       practice the restriction should be always there.

Within our synthesis algorithm, you are correct. However, Boomerang
includes more than Optician and permits lenses like id("a"|"a"). In
our extension to Boomerang, we do not remove this freedom.

                                -------------

     - L557 and below: Why some of the entropy is a single value while some
       of them seems ranging over an interval? If the interval is used to
       handle `merge_left`, then the entropy of all combinators should be an
       interval. Currently only `id`, `disconnect`, and `merge_left` yield
       intervals.

We were attempting to make our equations more readable, but we did not
sufficiently explain our syntax. The calls to H(...) return pairs,
and when multiplied by constants, or added together, they act as follows:

  a * (b,c) = (a * b,a * c)
  (a,b) + (c,d) = (a + b,c + d)

We will explain this more clearly in a final version of the paper.

                                -------------

     - L718 and L719: Different kinds of data definitely have different
       shapes. I cannot understand why assigning a fixed probability will fit
       in all situations. Could you explain it a bit?

See the shared section at the top of this response.

                                -------------
                             
     - L866 - L868: The user provides two REs describing the source and view
       as input, which should already give some useful structure. For
       example, the top-level nonterminal may be described by several other
       non-terminals. This structure indeed suggests that a big problem can
       be divided into several subproblems.  Why do the user still need to
       manually "generate subproblems"? Is it avoidable?

We do not believe such subproblems are straightforwardly
avoidable. While we can attempt to match up subcomponents, this
matching will often not work, for example in cases where various
equivalences must be applied (such as: ("a"|"b")|("c"|"d") <->
"a"|("b"|("c"|"d"))). If we had attempted to break down the problem
syntactically, we could have wasted a lot of time trying to find a
good lens that doesn't exist.

                                -------------
                             
     - L900: Before introducing the benchmark suite, the authors had better
       provide the specification, regarding CPU, memory, etc., of their
       benchmark environment since an important measurement is the speed of
       the synthesis process.

All benchmarks were performed on a 2.5 GHz Intel Core i7 processor with 16
GB of 1600 MHz DDR3 running macOS High Sierra. We will add this
information. 

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
benchmarks had to be altered to be bijective because of projected data, in
the symmetric benchmark suite we removed such alterations.) Of the 9
benchmarks that required the termination parameter, 4 of them required only
bijective lenses.

                                -------------
                             
     - L922: The authors may add a few words on how representative and
       complex the selected tasks is. For example, I got the data from the
       paper *Deep API Programmer: Learning to Program with APIs* that there
       is 238 real-world FlashFill benchmarks; then why the 8 data cleaning
       tasks here are more representative among the 238 tasks? The reviewers
       (and readers in the future) may want to know to which extent the job
       of writing lenses can be automatically done.


We chose our benchmarks from the FlashFill paper because the fuller
set of 238 tasks are not publicly available.  

Many real-life file format descriptors involve large numbers of
disjunctions and nested iterations. FlashFill struggles with
disjunctions and does not handle nested iterations. In our previous
work on synthesizing bijective lenses, we compared FlashFill to our
bijective synthesis algorithm in more detail, finding that FlashFill
was able to synthesize just 5 of the 39 examples that we had at that
time because of nested iterations.  In future version of this paper,
we would be happy to compare our work with FlashFill in greater
detail.

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

See our response "above the fold" for a description of our process.

Without an existing lens to compare to and without an existing set of
input/output examples, it is not obvious how to make the process of
checking correctness more automatic and objective.

We are interested in finding ways to make validating lenses easier for the user,
for example by finding examples of puts and creates that differentiate between
highly ranked lenses. However, we leave this task for future work.

                                -------------
                             
     - L945 and L993 (Fig.6 and Fig.7): Since the time is only within 30
       seconds, why not make Figures whose x-axes are linear? With the
       "non-linear figures", I cannot tell that SS took 1.3 times longer than
       BS. In addition, since we only care about the average time of tasks
       (and the maximum time of a single task), there is no reason to use a
       line chart, whose shape is drastically affected by the order of
       tasks. A simple table is better, if the details of tasks (and their
       order) are not given.

We chose a logarithmic x axis to show the differences on the fast
benchmarks, as they otherwise would be imperceptable. However, if the impact
on comparing the slower benchmarks is too significant, we can change to a
linear x axis.

Our graph does not include any specific order of tasks. At any given
time, a reader can see how many of the benchmarks are able to complete
within that time (for example, 41 benchmarks complete in under a
second in the full synthesis algorithm). We can reword the description
of these graphs to make this more clear.

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

We will update the lens portion of the related work section to include
a more thorough deription of related work that we do not make formal
statements about, and we will include the relevant citations.

                                -------------

     - L1111 and L1112: I cannot figure out where does `x` below the
       "horizontal line" come from

This is true for any arbitrary x (as that x gets "overwritten" by the user
provided x')


===========================================================================
Review #43C

    - The usability aspect is questionable: It seems that if the user has 
      defined complex regular expressions defining the pair of languages, then
      they can probably go ahead and just implement the lens without resorting 
      to examples.
      
As a demonstration of the difficulty of writing lenses, consider the
following bidirectional program
      
     let single_author_convert =
       ins " au - "
         . lens_swap
             (NAME . del ",")
             (lens_swap WSP NAME)*
     in ...

Even this relatively simple lens that permutes the 3 elements needed
for single_author_convert is fairly complex.  The complexity becomes
even more apparent when working with large lenses built from many
sub-lenses.  Lenses provide great power with their invertibility
guarantees, but they come at the cost of thinking about fiddly details
when writing the terms.  Worrying about these details while ALSO
thinking about unambiguity restrictions is a difficult task!

Moreover, engineering best practices suggest that it's a good idea to annotate lenses
with their types.  While Boomerang does not require these annotations (it
can infer the types from the terms), types serve as documentation for future
programmers to understand the formats that lenses process. Furthermore,
the types provide resilience in the face of future lens modifications,
ensuring that the lens maps exactly between strings of the provided
formats.  Hence, if lens authors are going to write down the types
anyway, why not allow our system to synthesize the fiddly terms from
those types for the programmer?  It seems clear work will be saved.

                                -------------
                                
    - More important: It is unclear to the general reader why the Expand
      procedure is needed — i.e., why do we need to rewrite types for
      GreedySynth.

Expand is needed because GreedySynth alone does not account for regular
expression equivalences. If given the two regular expressions "" | "a"+, and
"a"\*, GreedySynth alone will not synthesize the right lens:  it will
merge "" to "a"\* (with a disconect), and merge "a"+ (also with a
disconect).  The Expand algorithm searches for semantically equivalent
REs that are "compatible" in the sense that they allow GreedySynth to
find additional lenses between them.  In particular, in the example
given here, using Expand allows us to find a bijective lens.

                                -------------
                                
    - The ToStochastic procedure felt quite ad hoc to me. The paper makes a 
      point that if the user knew the distribution (e.g., if they automatically
      infer the parameters from data), then they can supply it. But will that
      really help the search? Essentially, I wish there were examples (even 
      synthetic ones) where knowing the distribution of data can clearly 
      influence the synthesis procedure.

See the shared section at the top of this response.

We can provide an example, like the one in the main response above, in the paper.

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

See the main response at the top of this response.

35 of the 48 symmetric benchmarks synthesized the same lenses as in the
Synthesizing Bijective Lenses paper. 4 of the benchmarks in Synthesizing
Bijective Lenses had to be altered to be bijective to account for projecting
information: those 4 were altered to their original forms, and we built our
algorithm on those 39 benchmarks. The remaining 9 benchmarks were added without
any changes to the synthesis algorithm (although we did make bugfixes while
adding those benchmarks). The 39 benchmarks from Optician hence were
not hand-picked, and our synthesis algorithm was not overtuned to the
remaining 9 benchmarks.

                                -------------
                                
    - Section 6 or 7 should discuss examples that are outside the expressivity 
      of the lens language and/or take too much time. It was hard for me to 
      infer what the limitations of the approach are.

We agree that section 7 requires more, informal comparisons to other lenses, for
us to discuss comparisons between our framework and other types of lenses. In
general, for different types of lenses to work, we require additional work,
either in the synthesis algorithm (for synthesizing different types of
combinators like matching lenses or delta lenses), or in both the synthesis
algorithm and specifications (like for edit lenses and update lenses).

                                -------------

    - My main concern is whether the use of stochastic REs is really useful (see
      above)
      
See the shared section at the top of this response.

                                -------------
                                
    - Are stochastic REs new? I didn't see any citations when they were 
      introduced.

Stochastic REs are not new. We cite them when we first mention them on page
2, but agree we should provide additional citations when they are formally
introduced. We do believe our syntactic technique for calculating their
entropy is novel.
