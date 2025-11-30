:- discontiguous noun_phrase/2.
% Basic grammar rules
sentence --> noun_phrase, verb_phrase.
noun_phrase --> determiner, noun.
verb_phrase --> verb, noun_phrase.

% Lexicon
determiner --> [the].
determiner --> [a].

noun --> [cat].
noun --> [dog].
noun --> [fish].
noun --> [bird].

verb --> [eats].
verb --> [sees].

% Adjectives extension
adjective --> [big].
adjective --> [small].
adjective --> [angry].

adjectives --> [].
adjectives --> adjective, adjectives.

% Updated noun_phrase with adjectives
noun_phrase --> determiner, adjectives, noun.