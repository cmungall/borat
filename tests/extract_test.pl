:- begin_tests(basic).

:- use_module(library(borat)).
:- use_module(library(borat/utils)).
:- use_module(library(borat/kboom)).

:- debug(extract).

links2axioms(L,A) :-
        findall(subClassOf(X,Y),member(X-Y,L),A).

test(extract_A) :-
        Links=[
               a-b,
               b-c,
               c-d,
               d-e,
               m-n,
               n-o,
               o-p
              ],
        links2axioms(Links,Axioms),
        
        extract_module([a],Axioms,ModAxioms),
        maplist(writeln,ModAxioms),
        assertion( ModAxioms = [subClassOf(a,b),_,_,_] ),
        
        extract_module([m],Axioms,ModAxioms2),
        maplist(writeln,ModAxioms2),
        assertion( ModAxioms2 = [subClassOf(m,n),_,_] ),

        writeln('**PAIR**'),
        extract_module([a,b],Axioms,ModAxioms3),
        maplist(writeln,ModAxioms3),
        assertion( ModAxioms3 = [subClassOf(a,b),_,_,_] ),

        writeln('**JOIN**'),
        extract_module([b,n],Axioms,ModAxioms4),
        maplist(writeln,ModAxioms4),
        assertion( ModAxioms4 = [subClassOf(b,c),_,_,_,_] ),
        nl.

links2praxioms(L,A) :-
        findall(0.2-subClassOf(X,Y),member(X-Y,L),A).

test(extract_H) :-
        Links=[
               a-b,
               b-c,
               c-d,
               d-e,
               m-n,
               n-o,
               o-p
              ],
        links2praxioms(Links,Axioms),

        extract_module([a],Axioms,ModAxioms),
        maplist(writeln,ModAxioms),
        assertion( ModAxioms = [_-subClassOf(a,b),_,_,_] ),
        
        extract_module([m],Axioms,ModAxioms2),
        maplist(writeln,ModAxioms2),
        assertion( ModAxioms2 = [_-subClassOf(m,n),_,_] ),

        writeln('**PAIR**'),
        extract_module([a,b],Axioms,ModAxioms3),
        maplist(writeln,ModAxioms3),
        assertion( ModAxioms3 = [_-subClassOf(a,b),_,_,_] ),

        writeln('**JOIN**'),
        extract_module([b,n],Axioms,ModAxioms4),
        maplist(writeln,ModAxioms4),
        assertion( ModAxioms4 = [_-subClassOf(b,c),_,_,_,_] ),
        nl.


:- end_tests(extract).
    
