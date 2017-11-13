:- module(utils,
          [write_solution/1]
         ).

%! write_kb(+Kb)
%
% 
write_solution(kb(Axioms,_,On,Pr)) :-
        length(Axioms,Len),
        format('+~w // WORLD=~w // Pr=~w~n',[Len,On,Pr]).
