
# Le Thesis

This is my graduation thesis, feel free to poke around and tell
me what an idiot I am.

It comes out of a project I slowly started working on a while ago: https://github.com/Swizec/Generative-poetry


# Le Goals

To implement a system that, given a corpora of poetry, can learn how
to write its own poetry on par so that a layman observer cannot
distinguish a generated poem from one written by a poet.

## Learning

Parameters to be learned:

* rhyming structure (phonetic match of last syllables in verse)
* assonance structure (vowel repetition for internal rhyming)
* alliteration structure (consonant repetition)
* rhythm
* verse structure
* stanza structure

Most of this basically requires a way to syllabify text, which I
believe can be achieved by finding a good substring cover of the
corpora with strings adhering to the C*V+C* pattern.

Semantically the system needs to

* sentence structure - supposedly well achievable with lexicalized
  probabilistic context free grammars
* semantic structure (at least for internal consistency)
* sentiment (emotional infliction of the poems)

## Generating

The above parameters of a poem should be constructed into some sort of
DNA from which poems can then be generated. It is important to achieve

* correct rhyming/assonance/alliteration or combination of these
* consistent use of rhythm
* appropriate division into verses and stanzas
* correct sentence structure
* internal semantic consistency

## Measuring success

It shouldn't be too difficult to measure correctness in terms of
proper rhyming etc. as long as we stick to strict poetic structures.

But this looks like an arduous manual process, so a better solution
needs to be found.
