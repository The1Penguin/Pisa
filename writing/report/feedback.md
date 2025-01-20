## From meeting
* Introduce Pisa in abstract
* Give a positive spin on the subset chosen in limitations.
* Some unclear / confusing its
* The reason we focus on translation is because the generation exists before.
* Introduce sorry keyword

## General
 * [x] There isn’t a clear research question and hypothesis. The introduction rather just says *what* you did, not why. There need to be motivation about why this is an interesting problem, and how it improves some aspect of the Lean proof assistant. Also, the hypothesis will hint at what evaluation you perform. The bare minimum for a MSc report is a clear testable hypothesis, and a matching evaluation which either refutes or confirms the hypothesis.

 * [x] Also, the introduction could even contain an high-level overview of your system, a bit like what you have in the beginning of chapter 3. This helps the reader understand what the thesis is about and why you explain what you explain in Chapter 2 (how this is relevant for the thesis)

 * [x] However, Chater 2, which contains background material, is probably not landing at the right level. Remember that your intended audience is a MSc student in your program, but who haven’t necessarily taken exactly the same courses as yourselves. I.e. you can’t assume the reader knows lots about type theory and Haskell. I also get the impression that you, in Chatper 2, mostly goes into the implementation of Lean, and not a “scientific” literature survey if you understand what I mean.

 * [x] You also very briefly gloss over QuickSpec and RoughSpec, which are the most important bits of related work, you even use RoughSpec later. Please properly explain how these two systems work, and how they discover conjectures, with examples. You should also mention Hipster and Hopster (similar to your work but for Isabelle and HOL)

 * [x] I didn’t quite get why you need the evaluation methods you describe in 3.2.2 and 3.2.3. It’s not clear if they play any role (and if so what that role is) in theory exploration, as you say nothing about them in section 3.3.

 * [x] As you set a too ambitious goal initially (translating dependent types), *I’d suggest you revise the goal to targeting theory exploration for a manageable subset of Lean*. You don’t need to present the report in the order you did the work either. You may use the main report to describe and report on the limited Lean subset that you can handle, and then in a part of the Discussion chapter discuss other things, such as why your original ambition didn’t work out (fully depended types etc)

 * [x] The Results chapter has hardly any results. You only test your tool on Bools and Nats. You ought to be able to find *some additional datatypes* that you can use it on? *Non-dependently types ones*, as that’s what it works for. You also need to evaluate the quality of the conjectures produced. [As discussed during the presentation, this ought to be possible. You could use for instance examples from QuickSpec, or even some other functional programming datatype that perhaps isn’t even formalised in Lean yet. This would help motivate why you would do theory exploration in the first place: to help with new formalisations]

 * [x] Finally, please rewrite the conclusion chapter. Go back and think in terms of a scientific hypothesis that you wish to refute/confirm. Did you manage to do that? You may also here reflect on what would be your recommendation to someone else trying to build a theory exploration system for Lean (as discussed in the presentation).

## Comments

 * [x] Page Iii (Abstract):
   > Mathematical proofs are becoming more intricate with time. This has resulted in a need for assistance during the proof creation, and to aid in this, interactive theorem provers were introduced.

   I don't know if I'd say that exactly, I think it's more that the demands on rigour has increased. 

 * [x] Page Iii (Abstract):
   > by adding tooling to include a conjecture theorem generator for the language

   The phrase "theorem conjecture generateor" is a bit funny. Either somehting is a conjecture (not proved) or it's a theorem (proved). Consider rephrasing.

   Also, say at least something about why this is useful! A little bit of motivation. What's the research question you set out to answer? 

 * [x] Page Iii (Abstract):
   > attempt to utilize Lean's dependent types was done, but was not able to be realized.

   Grammar. Rephrase.

 * [x] Page 1 (Introduction):
   > that is able to tailor itself to types of laws it aims to find, such as associativity, commutativity, or identity

   Not exactly tailoing itself. It's given templates describing the shape of a lemma. These are given as inputs. Your current phrasing makes it sound like it discovers them itself. Also, templates can be for any property, not just the ones listed here.

 * [x] Page 1 (Problem Statement):
   > The project aim was to create a Lean tool, Pisa, for generating conjectures. T

   This isn't a hypothesis that you can test. We will implement X, isn't sufficent for a problem statement. You will have to think about a phrasing that turns this into a testable hypothesis! Hint: It's going to be pretty much the same as for Hipster.

   This needs to be something that you can test in your evaluation section and either refute or confirm.

 * [x] Page 1 (Problem Statement):
   > achieve this, a source to source compiler, commonly referred to as a 1


 * [x] Page 2 (Problem Statement):
   > 1. Introduction transpiler, from Lean to Haskell was created

   This is *how* you implemented your system. Again, the problem statement is supposed to describe your hypothesis.

 * [x] Page 2 (Problem Statement):
   > Lean code produced by executing the code action on listing 1.1. Only realigned type declarations are shown to improve readability.

   This is impossible to understand for a reader unfamiliar with lean. Even I struggle to understand these conjectures. There are lots of weird variable names like X87, the notation B.t and B.f isn't explained etc. 

   If you want to show an example, perhaps a sanitised human readable version is more suitable for the introduction.

 * [x] Page 2 (Problem Statement): "#pisa 4 B B.t B.f not and, i" -- I don't understnad what this "pisa 4 B B.t. etc means at all. You need to explain this example so someone not familiar with Lean can understnad what you are actually asking your tool to do...

 * [x] Page 3 (Goals):
   > isomorphic representation without it and there are examples of doing that in a limited capacity (

   Can you give such an example?

 * [x] Page 4 (Goals): "Utilizing Lean's macro system t" -- As you haven't explained Lean's macro system yet, it's not easy to understnad what you mean here.

 * [x] Page 4 (Limitations):
   > Certain aspects of functions such as Input/Output (IO) will not be representable.

   But there's typically no I/O in a theorem prover? We're talking about formalised mathematics, not programming languages???

 * [x] Page 5 (Dependent types): "Without dependent types this type could not be verifiably expressed" -- Really? I'm pretty sure it could be proved by induction even with n and m having type nat, using suitable lemmas. 

 * [x] Page 6 (Dependent types):
   > Without dependent types the codomain has to change, such as wrapping the re

   You should probably explain what a codomain is. Recall that your intended audience is a student in your MSc program, not nessecarily having read exactly the same courses as you.

 * [x] Page 7 (Lean): "make a proper ITP" -- Have you explaind what ITP stands for? And what and ITP is? Remember that you write for an audience of other MSc students in your program, but who haven't nessecarily take the exact same courses as yourselves.

 * [x] Page 7 (Lean):
   > focus on generating relatively efficient C code (de Moura & Ullrich, 2021). This has enabled the compiler to be mostly bootstrappe

   This phrase makes it sound like the primary use case of Lean is program synthesis in C. I don't think that's the case.

 * [x] Page 7 (Example of syntax): "Haskell, all inductives are a sum of product types" -- Again, you might need to explain what sum and product types are. Recall your target audience

 * [x] Page 8 (Example of syntax): "themselves be parametric" -- also this concept might require a small explanation

 * [x] Page 8 (Example of syntax):
   > In Cons t Nil the type can be inferred to L B since the first argument type, α, is a B, w

   Consider rephrasing. What happens is that the type variable alpha is instantiated to bool. The way it's written now is quite unclear.

 * [x] Page 8 (Example of syntax):
   > Thus, what would be defined as a type alias in Haskell

   Note: the reader shouldn't be assume familiar with Haskell.

 * [x] Page 8 (Example of syntax): "avoid opening the inductive types scope." -- what does this mean?

 * [x] Page 9 (Example of syntax):
   > truth value of a proposition rely on there existing a constructor of said value, as explained in section 2

   This is not easy to understand. Remember your audience again.

 * [x] Page 9 (Extensible syntax):
   > An illustrative example is records in Haskell, where the semantics may hinge on relevant type information and data definitions. For instance, the code val {

   Why do you mix in examples from Haskell when you try to explain something about Lean? this section is quite confusing.

 * [x] Page 10 (Typed lambda calculus): "lication, lambda, and variable constructors one would expec" -- Again, your imagined audience is an arbitrary MSc student in your program. Do everyone know what De Bruijn indicies are?

 * [x] Page 10 (Typed lambda calculus): "is for sort that is the resulting type is." -- grammar needs fixing and you haven't explained the differnece between sorts and types.

 * [x] Page 10 (Typed lambda calculus): "motive, ther" -- what's a motive?

 * [x] Page 11 (Intermediate representation):
   > 𝜆Pure follows the style of A-Normal form, and 𝜆Rc adds reference counting in order to create optimizations.

   Either say more, or leave this out if it's not that relevant.

 * [x] Page 11 (Intermediate representation):
   > usually, be encoded as a constructor, which has a tag to identify what type it is.

   Is this relevant to theory exploration?

 * [x] Page 11 (Intermediate representation):
   > In contrast to how listing 2.7 would be encoded, listing 2.8 would be encoded as a unsigned integer.

   Again, how is this relevant for theory exploaraton?

 * [x] Page 11 (QuickSpec):
   > QuickSpec is able to test its way to conjectures using said functions.

   You gloss over the relevant stuff! *How* does QuickSpec do this? This is much more interesting and relevant that minor details about it's internal handling of types.

 * [x] Page 12 (QuickSpec):
   > The code will not compile, due to monoType part which points to no instance of Ord A, but that is not entirely true.

   But the point with QuickSpec isn't to generate code that compiles? It's to so theory exploration.

 * [x] Page 13 (QuickSpec):
   > That will result in the following code, seen in listing 2.12, instead of what was before The usage of this and the need to replicate a similar structure is further explained in section 3.2.3.

   missing full stop and general lots of focus on a minor detail.

 * [x] Page 13 (RoughSpec): "2.3.1 RoughSpec RoughSpec (Einarsdóttir et al., 2021" -- Insufficient! You write nothing about the most relevant pieces of related work, namely other theory exploration systems. I suggest dropping a lot of your discussions about low level details of type systems and focus on the main topic: theory exploration!

 * [x] Page 14 (Technical Specification):
   > The given definitions and all their transitive dependencies are resolved and serialized by the exporter.

   Please provide a worked example.

 * [x] Page 14 (Technical Specification):
   > Conjectures are materialized in the ITP upon request from the user by inserting the output of this executable.

   I don't understand if this means the process is or isn't automated. Does the user have to manually paste conjectures back into lean? Or not? What do you mean by "materialised"?

 * [x] Page 15 (Exporter):
   > arbitrary Lean syntax is non-trivial due to potential user defined parser extensions as explained in section 2.2.2

   For a MSc thesis is perfectly fine to restrict yourself to the base syntax and not worry about user-defined extensions.

 * [x] Page 15 (Exporter):
   > A custom export tool was therefore created. Given a list of identifiers, it first resolves the minimum complete set of required definitions.

   Maybe not for discussion exactly here, but somewhere you should discuss how your work differ/is similar to the work on Hipster (theory exploration wiht QuickSpec in Isabelle) and also I belive there's a prior MSc theis on doing this for the HOL ITP. 

 * [x] Page 17 (Translation):
   > Further, each definition's type is also translated, but the type may be an arbitrary expression, which could have the same issue with dependent types.

   Couldn't you have simply restrcited your thesis to the subset of Lean that doesn't really use any dependent-types? The ought to have made translation easier.

 * [x] Page 17 (Translation):
   > This breaks down in certain circumstances. Traversing a construction, and it's type together would probably provide better information to identify when to suspend and what context to request. However, to properly inspect any type complete evaluation have to be implemented. And if the evaluation is implemented it may be

   Again, it would probably be a good idea to simply say that you restrict yourself to a suitable subset of Lean, to avoid having to deal with this.

 * [x] Page 17 (Evaluation of typed lambda calculus): "the definitional values was created." -- where does these definitional values come from? From RoughtSpec? What are they? I'm confused...

 * [x] Page 17 (Evaluation of intermediate representation):
   > Lean uses a Intermediate representation to evaluate code. An interpreter for this l

   I'm confused: why do you need to write your own intepreter for Leans intermediate representation? Weren't you just translating for Lean to Haskell? Why do you need to also interpret something in a special way?

 * [x] Page 18 (Evaluation of intermediate representation):
   > However, since the value translation functions are auto generated for inductive types, an equivalent is needed for them. The unsigned integers that the interpreter uses as values are encoded as a natural number. Th

   Have you re-implemented parts of QuickSpec in Lean? This makes it sound like it.

 * [x] Page 19 (Evaluation of intermediate representation):
   > Instead, the values of a type that is an enum, that is to say one that can be encoded as a unsigned integer, will appear before the other types. This means that a little fix in the from and to Val needed to be implemented to handle this.

   I don't quite understand what you mean here to be honest.

 * [x] Page 20 (Interpretation):
   > "Quick specifications for the busy programmer" by Smallbone et al., 2017

   Add a proper referenmce to Smallbone et al

 * [x] Page 20 (Interpretation): "Rosén and Smallbone, 2015" -- Refernces in same style as before, so it's included in BibTex etc and appear in your reference list.

 * [x] Page 20 (Interpretation):
   > uses con. con takes a string argument,

   The fact that it's called con in the code is totally irreleant here and just confusing. I know what it is as I'm familiar with QuickSpec, but you really can explan this without referring to con, which is just short for "constant". Sometimes, less is more.

 * [x] Page 20 (Conjecture generation):
   > With a list of names and associated functions, QuickSpec is first run, with a max term size given by the end user to give a foundation.

   rephrase to improve readability. sentence is clunky and somewhat backwards. 

 * [x] Page 20 (Conjecture generation):
   > This is similarly done as outlined in section 4 of "Template-based Theory Exploration: Discovering Properties of Functional Programs by Testing" by Einarsdóttir et al., 2021

   Use proper referenceing. Also it's your job to explain this, the thesis should be reasonably self-contained.

 * [x] Page 21 (Example):
   > As an example of what the different stages do, N and add will be used. First, the translation will generate a Haskell file that looks like listing 3.5. This is then sent to interpretation step, which saves Haskell file in temporary directory to load it. It executes and returns the value of sigs to the original process. sigs are then sent to the conject

   You need to rephrase this as it is hard to read. And what's "sigs"? Can you explain how things work without referring to minor implementational details? Try to explain it on a algorithmic level rather than on the code level. Think more like a scientific paper rather than a code documentation report.

   You need to flesh out this example.

 * [x] Page 23 (Results):
   > This chapter details the results of the technical specification presented in chapter 3 in regard to the goals outlined by section 1.2. Goals (A) to (C) relate to the ability to handle domains of different complexities. That primarily means the ability to convert Lean code to a representation RoughtSpec can explore and are hence discussed for each approach to transpilation. The goals (D) "filtering conjectures" and (F) "integrating the subsystems" are examined separately, while (E) "utilizing auto proof engines" lacks any results and is therefore omitted from the chapter.

   Here I would expect you to remind the reader about what your scientific hypothesis is, and how you plan to evaluate it. It's not supposed to be about just "testing that the system works"

 * [x] Page 23 (Transpilation capabilities):
   > Any foundational support for data types should resolve (A) "support for simple types". Such a foundational implementation could be generalized to complete (B) "support for polymorphism", and even further for (C) "support for dependent types".

   So is your hypothesis something about evaluating differnet approaches to transpilation?

 * [x] Page 24 (Translation):
   > nductive types generally can. The definitions of simple and polymorphic types contain only type signatures that are compatible with Haskell. Translation of these signatures is therefore straightforward. This method cannot be deemed to fulfill any of the stated goals, but the type translation is useful.

   OK, so you should have restricted yourself to theory exploation for this subset.

   You are allowed to change the original goals of the thesis! 

 * [x] Page 24 (Evaluation): "method described in section 3.2.3." -- the reader doesn't remember what the appraoch in this section was when they get here.

 * [x] Page 24 (Evaluation):
   > typed lambda calculus, issues arose with the recursor. The issues were with the order of evaluation, in

   Focus on the things that worked out. You don't need to mix in all things ytou tried that didn't in the end work. It makes the evaluation harder to read. What's interesting to the reader is your method that really worked, not all the paths that you went down getting there. 

   I ususally suggest that "failed attempt" can be discussed separately in e.g. a separate short section.

 * [x] Page 25 (Evaluation):
   > The definitions used in the call for Pisa in listing 4.1 would generate the conjectures seen in listing 4.2. In the output, one can see that laws one would expect appear, such as commutativity, associativity, and De Morgan laws.1

   You could present this not verbatin but in nice latex formatting, with sanitised variable name. You could then also number them and map the names to the right conjecture.

 * [x] Page 25 (Evaluation):
   > Using this approach, dependent types is not able to be used. The IR that is used for evaluation in this case could be able to run certain types of dependently typed code, such as vector where the length is kept track of. However, ensuring that invariants that are guaranteed isn't always possible for the generated code. For example, if a vector is defined in Lean, that keeps track of the size of the vector, Haskell would not be able to ensure that a Cons constructor is always one element larger. This issue can be seen how listing 4.7, which most reasonably be translated to listing 4.8, does not enfore the size of the vector. The size will onl

   Again, this isn't at all surprising that it's hard! As suggested, I recommend limiting yourself to the subset of Lean without dependent types.

 * [x] Page 26 (Filtering):
   > One difference between Lean and Haskell is how a function is defined. If a function doesn't properly define all the cases for a match in Lean it will not type check, and will not be able to be used. In Haskell, t

   This section contains no experiments? Maybe this should be part of implementation chapter?

 * [x] Page 26 (Integration of subsystem):
   > 4.3 Integration of subsystem The subsystem has been incorporated into a macro for use

   What's the experiment here? This section doesn't seem to belong in a Results section.

 * [x] Page 29 (Conclusion):
   > This implementation demonstrates the feasibility of assisted theorem exploration integrated in Lean. The goals that the project set out to accomplish partially were. To what extent is summarized by the breakdown in table 5.1

   Rewrite. You're not doing a feasibility study. You need to think in terms of scientific hypothesis.

 * [x] Page 29 (Conclusion):
   > Worth noting is that usage of an auto proof engine was not attempted due to time constraints resulting from the different approach

   I think you could easily add some evaluation of maybe even manually using something like aesop on the conjectures.
