:- module(utils,
          [write_solution/1]
         ).

%! write_solution(+Kb)
%
% 
write_solution(kb(Axioms,_,On,Pr,_)) :-
        length(Axioms,Len),
        format('+~w // WORLD=~w // Pr=~w~n',[Len,On,Pr]).
