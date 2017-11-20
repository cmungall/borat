:- begin_tests(lexinf_learn).

:- use_module(library(borat)).
:- use_module(library(borat/utils)).
:- use_module(library(borat/ptriple_loader)).
:- use_module(library(borat/lexinf)).

:- debug(rdf).

test(train) :-
        kb_from_rdf_file('tests/data/pc.ttl',Kb),
        forall(training_instance(A,B,I),
               writeln(i(A,B,I))),
        writeln('TRAINING'),
        forall(predict_relation(R,M,P),
               writeln(learn(R,M,P))),
        nl.

:- end_tests(lexinf_learn).
    
