:- begin_tests(solve).

:- use_module(library(borat)).
:- use_module(library(borat/utils)).

:- debug(search).


test(basic) :-
        Init=[
              ],
        H=[
           0.8-subClassOf(c,b)
          ],
        search(Init, H, Sols),
        writeln('*BASIC*'),
        maplist(write_solution,Sols),
        Sols=[_,_],
        % TODO
        nl.


test(subClass) :-
        Init=[
              max(300),
              in(a1,s1),
              in(b1,s1),
              in(c1,s1),
              in(d1,s1),
              in(a2,s2),
              in(b2,s2),
              in(c2,s2),
              in(d2,s2),
              all_unique(s1),
              all_unique(s2),
              subClassOf(b1,a1),
              subClassOf(c1,b1),
              subClassOf(d1,c1),
              subClassOf(b2,a2),
              subClassOf(c2,b2),
              subClassOf(d2,c2)
              ],
        H=[
           0.7-equivalentTo(a1,a2),
           0.8-equivalentTo(b1,b2),
           0.6-equivalentTo(c1,c2),
           0.2-disjointWith(a1,a2),
           0.3-subClassOf(a1,d2)
          ],
        search(Init, H, Sols),
        writeln('*SOLS*'),
        maplist(write_solution,Sols),
        member(subClassOf(d2,a),Out),
        member(subClassOf(d,a),Out),
        member(subClassOf(d,b),Out),
        member(subClassOf(d,c),Out),
        member(subClassOf(c,a),Out),
        member(subClassOf(b,a),Out).

test(conflict) :-
        Init=[
              not(subClassOf(x,y))
              ],
        H=[
           0.8-subClassOf(x,y)
          ],
        search(Init, H, Sols),
        writeln('*CONFLICT*'),
        maplist(write_solution,Sols),
        % we expect no solutions
        assertion( Sols=[] ),
        nl.

test(filter) :-
        Init=[
              subClassOf(b,a),
              subClassOf(c,b),
              % gap
              subClassOf(e,d),
              subClassOf(f,e)
             ],
        H=[
           0.99-subClassOf(d,c),
           0.05-subClassOf(f,a)
          ],
        search(Init, H, Sols),
        writeln('*FILTER*'),
        maplist(write_solution,Sols),
        assertion( Sols=[_,_,_] ),
        nl.



:- end_tests(solve).
    
