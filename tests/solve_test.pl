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
        % we expect two solutions one, for pos and negative
        assertion( Sols=[_,_] ),
        Sols=[Best,_],
        assertion( kb_S(Best,[_-subClassOf(c,b)]) ),
        nl.

test(nomerge_simple) :-
        Init=[
              in(a1,s1),
              in(b1,s1),
              all_unique(s1)
              ],
        H=[
           0.99-equivalentTo(a1,b1)
          ],
        search(Init, H, Sols),
        writeln('*NM_SIMPLE*'),
        maplist(write_solution,Sols),
        
        % we expect one solution
        assertion( Sols=[_] ),
        Sols=[Sol],
        
        % the equivalence solution is ruled out due to all-unique constraint
        assertion( kb_S(Sol,[_-not(equivalentTo(a1,b1))])).

test(nomerge_inf) :-
        Init=[
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
           0.9-subClassOf(a1,d2)
          ],
        search(Init, H, Sols),
        writeln('*NM_INF*'),
        maplist(write_solution,Sols),
        Sols=[Best|_],
        assertion( (kb_S(Best, BestAxioms),
                    member(_-not(subClassOf(a1,d2)), BestAxioms)) ),
        writeln(Best).

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
    
