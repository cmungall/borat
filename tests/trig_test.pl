:- begin_tests(rdfload).

:- use_module(library(borat)).
:- use_module(library(borat/utils)).
:- use_module(library(borat/ptriple_loader)).

:- debug(rdf).

test(load) :-
        kb_from_rdf_file('tests/data/simple.trig',Kb),
        writeln(Kb),
        kb_search(Kb, Sols, [una(local)]),
        writeln('*FROMOWL*'),
        length(Sols,Len),
        maplist(write_solution,Sols),
        assertion( Len = 3 ),
        Sols=[Best|_],
        kb_S(Best,L),
        L=[_,_],
        assertion( member(_-equivalentTo('http://example.org/a',
                                         'http://example.org/a2'),L)),
        assertion( member(_-not(equivalentTo('http://example.org/a',
                                             'http://example.org/b2')),L)),
        nl.

:- end_tests(rdfload).
    
