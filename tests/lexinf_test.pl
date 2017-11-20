:- begin_tests(lexinf).

:- use_module(library(borat)).
:- use_module(library(borat/utils)).
:- use_module(library(borat/ptriple_loader)).
:- use_module(library(borat/lexinf)).

:- debug(rdf).

test(load) :-
        kb_from_rdf_file('tests/data/lexmap_test.ttl',Kb),
        G=ann(_,_,_,_,_,_),
        forall(G,
               writeln(G)),
        functor(G2,lmatch,8),
        forall(G2,
               writeln(G2)),
        forall(lmatch(A,B),
               writeln(A-B)),
        nl.

:- end_tests(lexinf).
    
