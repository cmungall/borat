:- begin_tests(explain).

:- use_module(library(borat)).
:- use_module(library(borat/utils)).

:- debug(search).

/*

given:
  
  b ===== b2
  ^       ^
  |       |
  |       |
  a ..?.. a2

the evidence for an equivalence axiom between a and a2 is boosted by
the fact it explains the two subClassOf links (i.e. if we add a=a2,
  and remove either subClassOf, it is still entailed by deductive reasoning.
  
*/
test(explain) :-
        Init=[
              subClassOf(a,b),
              equivalentTo(a,a2),
              subClassOf(a2,b2)
              ],
        H=[
           0.4-equivalentTo(b,b2)
          ],
        search(Init, H, Sols,[use_explain(true)]),
        writeln('*BASIC4*'),
        maplist(write_solution,Sols),
        assertion(Sols = [_,_]),
        Sols = [Best,_],
        assertion( kb_S(Best, [_-equivalentTo(b,b2)]) ).


:- end_tests(explain).

