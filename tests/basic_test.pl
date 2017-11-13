:- begin_tests(basic).

:- use_module(library(borat)).
:- use_module(library(borat/utils)).

:- debug(search).

test(basic4) :-
        Init=[
              ],
        H=[
           0.8-subClassOf(b,a),
           0.8-subClassOf(c,b)
          ],
        search(Init, H, Sols),
        writeln('*BASIC4*'),
        length(Sols,Len),
        writeln(Len),
        maplist(write_solution,Sols),
        %assertion( Len == 4 ),
        assertion(Sols = [_,_,_,_]),
        % TODO
        nl.

test(basic6) :-
        Init=[
              ],
        H=[
           0.8-subClassOf(b,a),
           0.8-subClassOf(c,b),
           0.2-subClassOf(c,a)
          ],
        search(Init, H, Sols),
        writeln('*BASIC8*'),
        length(Sols,Len),
        maplist(write_solution,Sols),
        assertion( Len = 7 ),
%        \+ (member(S,
        % TODO
        nl.


:- end_tests(solve).
    
