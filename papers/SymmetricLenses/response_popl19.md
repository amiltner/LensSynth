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
the file should be synchronized to which.

In validating that the synthesized lenses were correct, we applied the types of
approaches we use in validating our code is correct in everyday development.
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

Paper summary
-------------
This paper investigates the automatic synthesis of symmetric lenses, i.e.,
programs that allow data to be synchronized (updated in both directions)
between two data sources, such as two databases. Specifically, this paper
identifies a well-behaving subclass of symmetric lenses, called simple
symmetric lenses, for which the authors are able to develop an efficient
synthesis algorithm. The main difference between simple and ordinary
symmetric lenses is that the former do not contain a notion of complement,
significantly reducing the data the user has to provide for synthesis.

In detail, this paper:

- Introduces a refinement of the notion of symmetric lens, called simple
  symmetric lenses, that are well-suited for synthesis purposes. The
  paper organises their definition into a smalltyped language that provides
  a set of combinators for building complex lenses from simpler ones.

- Proposes a (mostly, up to provided examples and tuning of parameters)
  automatic synthesis algorithm, which 1) first expands the given regular
  expressions into stochastic regular expressions, and 2) then tries to
  iteratively find a "likely" simple symmetric lens between stochastic
  regular expressions equivalent to the original ones. The algorithm
  terminates when certain preference and termination conditions are
  satisfied.

- Extends the standard notion of star-equivalence to stochastic regular
  expressions so as to compare and (soundly) rewrite them.

- Combines stochastic regular expressions with ideas from information
  theory, using the entropy of data sources to compute costs for lenses,
  so as to guide synthesis towards "more likely" candidate lenses, i.e.,
  those lenses that propagate a lot of information.

- Defines a notion of DNF stochastic regular expressions and a 
  corresponding notion of DNF symmetric lenses that are used 
  as an intermediate representation in lens synthesis.

- Incorporates the proposed synthesis algorithm into the Boomerang
  language and evaluates it on 48 benchmarks that cover a variety of
  real world data formats, ranging from converting Linux configuration
  files from ad hoc textual presentation to structured dictionaries to
  synchronising BibTex and EndNote citation descriptions.

  While the naive implementation of this synthesis algorithm was only
  able to find 26 of the required lenses, then a mildly optimised
  variant of it (supporting compositional synthesis) was able to
  synthesise good lenses for all 48 benchmarks, each under 30 sec.

  The paper also demonstrates that when restricted to bijective
  lenses, the proposed synthesis algorithm is roughly on bar with
  an existing dedicated synthesis algorithm for such lenses.

As I think the paper does a good job of combining theoretical and
practical research, e.g., by identifying a useful and large subclass
of symmetric lenses, and demonstrating that lenses in this class can
be efficiently (mostly automatically) synthesised for many real world
data formats using just a few examples. Also, as far as I can tell,
the developed mathematics is correct (modulo some typos listed below).
Thus I recommend this paper to be accepted at POPL 2018. However, as
I am not an expert on program synthesis, I will leave scrutinising
the related work and this work's relationship to it to experts.

Strengths
---------
The paper does a good job of combining theoretical and practical
research, e.g., by identifying a useful and large subclass of
symmetric lenses; by using information-theoretic ideas to define
a notion of lens cost that can be used effectively for candidate
selection in synthesis; and by demonstrating that the proposed
algorithm works well on various real world data formats.

Weaknesses
----------
I have no significant complaints about this paper apart from Section 3
not making it clear that stochastic regular expressions are not a
contribution of the authors (albeit it is mentioned in passing on p2).
My other comments are mostly about layout and (what are hopefully) typos.

Comments for author
-------------------
Comments:

1. It is usually a good style not to include citations in abstracts.

2. You should mention Optician when referring to the work of Miltner
   et al. in the Introduction. Otherwise its mention in the beginning
   of Section 6 kind of comes out of thin air.

3. p5 l236,

   Shouldn't the second argument to `disconnect` be `company`?

4. p6, l288,

   If I understood correctly, this parituclar lens `disconnect(...)`
   picks the default values at random, but is still valid because it
   satisfies the required lens type. If so, please make it clearer.

5. p7, l321, 

   At this point the $\mathbb{H}$ notation for entropy has not yet
   been introduced, perhaps introduce it above for `S` and `P`.

6. p9, l438,

   The combination of `S` and the entropy formula in the parentheses
   looks a bit like some sort of function application, please make
   it clearer that it isn't.

7. p10, l449

   It might be worth reminding the reader what unambiguous regular
   expressions are.

8. p16, l736,

   It could be worthwhile pointing out explicitly that the structure 
   of symmetric DNF lenses mirrors exactly that of SDNF REs.

9. p16, l777,

   Could you give concrete examples of situations where all sequences
   are involved in sequence lenses, but there are examples that could
   make other lenses more useful?

10. p18, l842

    Could you explain in some more detail what criteria does `EXPAND` use
    to decide to open some closed expressions but not others?

11. p19, l887

    Surely the definition of `CONTINUE(pq)` should be using `max`? Otherwise
    I don't see how the definition makes sense, because using surely `min` 
    will just pick 0 for t>=0, because the distance and the logarithm are
    positive values.

12. p21, l1022
  
    Which preference metric are the t=0, t=25, t=-25 benchmarks using?


Typos:

- p3, l119, "will depend which" -> "will depend on which"

- p5, l220, "we shoren" -> "we shorten"

- p5, l230, "putR s1 s2 = s2" -> "putR s1 s2 = s1"

- p8, l360, "GREEDYSYNTH converts yields" -> "GREEDYSYNTH yields"

- p9, l414, "annotation p how" -> "annotation p represents how"

- p10, l478, "typing derivations" -> "typing rules"

- p11, l478, "these lenses" -> "these combinators"

- p12, l565, "H->(T|l1,S1) + H->(T|l2,S2)" -> "H->(T1|l1,S) + H->(T2|l2,S)"

- p12, l573, "uncommon on practice" -> "uncommon in practice"

- p12, l575, "maximum sum of recovering" -> "sum of maximums of recovering"

- p12, l583, "definition of H contains" -> "definition of H-> contains"

- p14, l657, "SDNF REs are SDNF REs" -> "SDNF REs are DNF REs"

- p15, l715, "expression., " -> "expression, "

- p15, l721, "syntactic means of" -> "syntactic means for"

- p15, l723, "DNF SRE" -> "SDNF RE"

- p17, l820, "pair of stochastic atoms" -> "pair of lists of stochastic atoms"

- p19, l926, "runs the tools, determines" -> "runs the tool, determines"

- p21, l1100, "simple lenses" -> "simple symmetric lenses"

Questions for authors’ response
---------------------------------
First, I would really like if the authors could provide answers to
questions 9, 10, 11, and 12 above.

Second, from what I remember from the literature on (ordinary) symmetric 
lenses, they do not lend themselves very well to analysis in terms of 
spans of asymmetric lenses. With simple symmetric lenses, do you think 
the situation is better now that you have omitted the complement part?

Thirdly, do the authors expect to be able to extend the proposed synthesis
algorithm to handle any of the many variants of updates-based lenses, 
such as edit lenses, delta lenses, and update lenses?




===========================================================================

Review #43B
Paper summary
-------------
The paper proposes simple symmetric lenses --- symmetric lenses that
are forgetful, i.e. the complement can be determined by the most
recent “source” and “view” --- and believe they are practically
useful. Different from traditional approaches, after defining basic
combinators (of simple symmetric lenses), the paper votes for
synthesising lenses instead of manually writing them. To do so, the
paper proposes a novel SRE and information-theoretic based approach,
which makes it stand out from the previous synthesising work on
(quotient) bijective lenses. The evaluation is based on the previous
Optician benchmarks including 48 tasks. With all optimisations turned
on, it is able to finish each work in 30 seconds. The paper also
discusses the relationship with asymmetric lenses and symmetric
lenses, and proves that simple symmetric lenses lie in between them.

Strengths
---------
- The SRE and information-theoretic based synthesising approach is
  novel and may have much influence on follow-up research.

- The evaluation shows that the authors' approach has very fast speed
  on the tasks adapted from other real world application. (One
  problem of program synthesis is its low speed.)

- The appendix provides details about the specification of simple
  symmetric lens combinators and proofs of theorem in the paper.

Weaknesses
----------
- The main weakness is in the evaluation part. There are many
  unclear/controversial parts regarding the evaluation. For example,
  the lack of description of the benchmark environment; the way they
  test whether the synthesised lens is correct; the description of the
  tasks to be synthesised. See detailed comments below. (This defect
  does not prevent it from being a good paper.)

- The first subsection of Related Work did not give any reference to
  the related work except for Foster et al. 2007, which makes the
  paper unfriendly and hard to verify. The appendix is also in a
  messy.

Comments for author
-------------------
L11: The reference Martin Hofmann et al. (2015) does not help since it
is not yet published and cannot be obtained from Internet. Martin
Hofmann et al. (2011) may be the choice for this time.

L230: `putR s1 s2 = s2`  --> `putR s1 s2 = s1`  

L238: `createR s = "unk"` --> `createL _ = "unk"`  
These two errors will be misery for non-experts.  
BTW, the authors sometimes use `s` and `v`, sometimes use `s1` and `s2`. It will be better to use a uniform notation.

L401 and L411: `ϵ` is not in the grammar of SRE; it appears without
any explanation.

L442: H(s) should be 0. Otherwise the entropy of the SRE in L452 is 2.

L641 and L642: previously they are `S*p -> ϵ |1-p (S・S*p)` and `S*p
-> ϵ |1-p (S*p・S)`, instead of `S*p -> ϵ |p (S・S*p)` and `S*p -> ϵ
|p (S*p・S)`.


L484 and L485:
I cannot see the reason to relax the restriction regarding unambiguous
REs. What will happen if primitive lens combinators for unambiguous
REs are composed with the ones for ambiguous REs? Since complex lenses
are composed from (ambiguous and unambiguous) primitive lenses, in
practice the restriction should be always there.

L557 and below: Why some of the entropy is a single value while some
of them seems ranging over an interval? If the interval is used to
handle `merge_left`, then the entropy of all combinators should be an
interval. Currently only `id`, `disconnect`, and `merge_left` yield
intervals.

L584 ~ L606: From some other perspective, if the overall effect is
just a bijection, there is no reason to further decompose the
bijection into non-bijective ones in practice.

L718 and L719: Different kinds of data definitely have different
shapes. I cannot understand why assigning a fixed probability will fit
in all situations. Could you explain it a bit?

L747: I skimmed this subsection and believe that the algorithm is
correct (since there is an implementation). There are too many small
caps in the algorithm figures.

L866 - L868: The user provides two REs describing the source and view
as input, which should already give some useful structure. For
example, the top-level nonterminal may be described by several other
non-terminals. This structure indeed suggests that a big problem can
be divided into several subproblems.  Why do the user still need to
manually "generate subproblems"? Is it avoidable?

L900: Before introducing the benchmark suite, the authors had better
provide the specification, regarding CPU, memory, etc., of their
benchmark environment since an important measurement is the speed of
the synthesis process.

L909 and L956: About the benchmark suite, how many of the benchmarks
in fact only need bijective lenses? This definitely should be
considered since (1) the benchmark suite is adapted from the "original
Optician benchmarks, comprised of 39 bijective synthesis tasks", and
(2) "Adjusting the termination parameter was needed in only 9 of the
48 tasks". I suppose it is not, but still wonder, if for all of the 48
- 39 = 9 non bijective tasks, we need to adjust their termination
parameters?

L922: The authors may add a few words on how representative and
complex the selected tasks is. For example, I got the data from the
paper *Deep API Programmer: Learning to Program with APIs* that there
is 238 real-world FlashFill benchmarks; then why the 8 data cleaning
tasks here are more representative among the 238 tasks? The reviewers
(and readers in the future) may want to know to which extent the job
of writing lenses can be automatically done.

L926 and L952: This is the most confusing point for me. The meaning of
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

L945 and L993 (Fig.6 and Fig.7): Since the time is only within 30
seconds, why not make Figures whose x-axes are linear? With the
"non-linear figures", I cannot tell that SS took 1.3 times longer than
BS. In addition, since we only care about the average time of tasks
(and the maximum time of a single task), there is no reason to use a
line chart, whose shape is drastically affected by the order of
tasks. A simple table is better, if the details of tasks (and their
order) are not given.

L1055: Again, if the default (termination) parameter can only find
bijective lenses, it is not that good.

L1066: When discussing symmetric lenses, the authors need to give a
reference to Symmetric Lenses by Martin Hofmann et al. (2011) since
the definitions and notations are from there.

L1079: The authors silently move to introduce edit lenses, although
different from Edit Lenses by Martin Hofmann et al. (2012), a short
introduction and a reference are needed. Moreover, the authors may
need to explicitly mention that the edits in this section are
"overwrites"; otherwise it is difficult to understand why the edits
have the same type as the source and view (ignoring the sum types made
by inl and inr.), as usually they are some "deltas".

L1080: The authors did not explain the notation `l ∈ X ⟷ Y` as it
hides the complement `C`.

L1105 and below: Similarly, the authors need to explain the inference
rules and the involved symbols more. For instance, what is `xyo`?
There is even no note telling the reader that some explanation can be
found in the appendix.

L1111 and L1112: I cannot figure out where does `x` below the
"horizontal line" come from

L1170: I do believe information-theoretic analysis is of extremely
useful; if possible, adding one more example will make the argument
stronger.


##Things that hinder reading

L262: this is the first time that "forward direction"
appears. Besides, for symmetric lenses, it is unclear what is the
forward direction.

L364: `... within emp_salaries ...`. According to the context, this
`emp_salaries` is supposed to to be found in Figure 3 and there
happens to be a lens type called `emp_salaries`. But in fact the
`emp_salaries` should refer to the one in Figure 2.

L659: The notation (DS,DT) might need some explanation. It is not a
tuple but just introduce two preferable names DS and DT.

L1101 and L 1103: the authors use "edit `esi`" here but use "edits
`esi`" at L1082. Is `esi` a single edit or a sequence of edits? Having
read it many times, I eventually understand that it should be the
single edit that we focus on; so that `esi = inl x`.

L1509 and L1515: the two lines exceed page limit. Luckily the exceeded
parts can be guessed.

## minors:

L14: that -> than

L54: Indeed of -> Indeed among

L220: shoren -> shorten

L372: between -> among

L414 and L415: some words are missing in the sentence so that it is
not easy to understand.

L426: a means to reason -> a means of reasoning

L431: present -> presented ?

L438: the authors had better to put the definition of entropy
somewhere earlier in this sentence. Otherwise it syntactically looks
like the definition of data source S.

L451: a redundant ",s"

L453: "in subtle ways, the algorithm" -> "in subtle ways and the algorithm"

L459: I do not understand the example. Which law gives the result?

L542: combonator -> combinator

L547: equivalent -> is equivalent

L553: does `H->(l,s | T)` looks better than the original `H->(T | l,s)`? There is no reason to use the same argument order for `H->` and `H<-` since we can think them as different functions.

L620: I cannot parse the sentence

L656: SDNF REs are SDNF REs with probability annotations -> SDNF REs
are DNF REs with probability annotations.

BTW, the authors had introduced the abbreviation SRE in the
introduction. But in this subsection the authors use too many
different abbreviations:

```
SDNF REs
stochastic DNF REs
stochastic DNF regular expressions

SREs
stochastic REs
stochastic regular expressions

REs
regular expressions
```
Please consider use the same notation throughout.

L658: `DS bar` is introduced but seems to be never used.

L715: expression., -> expression,

L734: than there are DNF bijective lenses -> than DNF bijective lenses

L845: uncoverted -> unconverted

L928: whether it is usable to generate synthesize correct lenses ->
whether it is usable to synthesize correct lenses

L929: I prefer to remove the "the" in the first sentence.

L971: measuring it shows of the responsiveness -> measuring it shows
the responsiveness

L1080 ~ L1082: the description of the function apply is its curried
version while the usage below is the uncurried version.

L1082: edits `esi` -> edit `esi`

L1099: I guess the title should be "Relation with Symmetric Lenses"

L1099: classic -> classical

L1136: `l.put s v` -> `l.put v s`

L1138 `l.put s (l.get s)` -> `l.put (l.get s) s`

L1512: I found `(y,c2') = l.putR (x2', c1')`, do you mean `(y,c2') =
l.putR (x2', c2')`

L1526 and L1530: sometimes x and y are used and sometimes s and v.

L1702: this section is not FORGETFUL SYMMETRIC LENSES (which is the
previous section)

L2088: perhaps the theorem about lenses should be put to the previous
section. This section is about SRE and information theory.

L2098: this is CREATEGET, not PUTGET again

L2112: `l.get` -> `l.get s`

Questions for authors’ response
---------------------------------
Please clarify the evaluation part.


===========================================================================
Review #43C

Paper summary
-------------
- This paper proposes a technique for synthesizing a form of lenses from input-output examples and types. 
- Lenses are transformations from one data type to another. They are typically used to transform between data formats, e.g., CSV to JSON.
- Previous work on synthesizing lenses covered the bijective case, where the synthesized lens is not lossy.
- This paper builds on top of prior work to synthesize lossy lenses, which they call simple symmetric lenses. These lenses allow data transformations that lose data, e.g., extracting one column from a table.
- Compared to bijective lenses, the search space is less constrained. The observation of previous work was that the bijectivity constraint is so restrictive that any lens synthesized is likely to be the right one. By removing this constraint, the search space is much larger.
- To work around that restriction, the authors propose to use probabilistic types, which define a probability distribution over inhabitants of the type. Then, they propose the heuristic where they look for lenses that minimize entropy in the transformation — roughly the number of bits needed to recover one format from the other.
- The synthesis algorithm largely follows that from POPL18 on bijective lenses — types are defined via regular expressions, regular expressions are unrolled (expanded) into DNF form, and then a greedy search looks for a lens for that pair of types.
- The experiments demonstrate the usefulness of the search heuristics used—particularly using entropy to guide the search.

Strengths
---------
+ Interesting application of synthesis
+ Elegant set of combinators defining simple symmetric lenses
+ Intriguing information-theoretic search heuristic

Weaknesses
----------
— Small algorithmic delta over POPL18

— Unclear whether stochastic REs are needed

Comments for author
-------------------
I enjoyed reading this paper. I think the technique is elegant and well-reasoned. The usability aspect is questionable: It seems that if the user has defined complex regular expressions defining the pair of languages, then they can probably go ahead and just implement the lens without resorting to examples. 

While the paper begins with a nice high-level illustration and examples, the technical sections lack illustrative examples and motivation that it’s hard to appreciate some points. For example, Section 4.1 introduces the lens cost computation, but no example of such computation is given. More important: It is unclear to the general reader why the Expand procedure is needed — i.e., why do we need to rewrite types for GreedySynth. 

The ToStochastic procedure felt quite ad hoc to me. The paper makes a point that if the user knew the distribution (e.g., if they automatically infer the parameters from data), then they can supply it. But will that really help the search? Essentially, I wish there were examples (even synthetic ones) where knowing the distribution of data can clearly influence the synthesis procedure. The paper resorts to a fixed distribution that gives the same probability to each disjunct. This raises two concerns:
 

1.  that this overfits to the hand-picked benchmarks and 
2.  that the entropy calculation and stochastic REs are an overkill. 

Why not use a syntactic heuristic. That’s what DC is trying to do, but perhaps a slightly more sophisticated syntactic heuristic could achieve the same effect. So, while the use of entropy is cool, appealing, and new in synthesis (to my knowledge), the paper does not convince me that it is needed here.

Section 6 or 7 should discuss examples that are outside the expressivity of the lens language and/or take too much time. It was hard for me to infer what the limitations of the approach are.

Questions for authors’ response
---------------------------------
My main concern is whether the use of stochastic REs is really useful (see above)

Are stochastic REs new? I didn't see any citations when they were introduced.
