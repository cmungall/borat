:- begin_tests(solve).

:- use_module(library(borat)).

:- debug(search).

write_kb(kb(Axioms,_,On,Pr)) :-
        length(Axioms,Len),
        format('+~w // WORLD=~w // Pr=~w~n',[Len,On,Pr]).

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
        maplist(write_kb,Sols),
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
        writeln('*SOLS*'),
        maplist(write_kb,Sols),
        % we expect exactly one solution
        Sols=[_],
        nl.

mk_ax(equivalentTo(X,Y),X,Y).
mk_ax(subClassOf(X,Y),X,Y).

% TODO: filterout axioms that cannot be true once selected
test(filter) :-
        Init=[
              subClassOf(b,a),
              subClassOf(c,b),
              % gap
              subClassOf(d,c),
              subClassOf(e,d)
             ],
        H=[
           0.99-subClassOf(b,c),
           0.01-subClassOf(e,a)
          ],
        search(Init, H, Sols),
        writeln('*FILTER*'),
        maplist(write_kb,Sols),
        nl.


:- end_tests(solve).
    
