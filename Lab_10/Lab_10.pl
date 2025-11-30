:- discontiguous noun_phrase/3.
% Core Semantic Grammar
sentence(Sem) --> noun_phrase(Subj), verb_phrase(Subj, Sem).

noun_phrase(Subj) --> determiner, noun(Subj).

verb_phrase(Subj, Sem) --> verb(V), noun_phrase(Obj), { Sem =.. [V, Subj, Obj] }.

% Lexicon with Semantics
determiner --> [the].
determiner --> [a].

noun(cat) --> [cat].
noun(dog) --> [dog].
noun(fish) --> [fish].
noun(bird) --> [bird].

verb(eat) --> [eats].
verb(see) --> [sees].

% Extension: Adjectives
noun_phrase(Subj) --> determiner, adjectives, noun(Subj).

adjectives --> [].
adjectives --> adjective, adjectives.

adjective --> [big].
adjective --> [small].
adjective --> [angry].