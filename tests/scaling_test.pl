:- begin_tests(scaling).

:- use_module(library(borat)).

:- debug(search).

kb_prob(kb(_,_,_,Pr), Pr).

write_kb(kb(Axioms,_,On,Pr)) :-
        length(Axioms,Len),
        format('+~w // WORLD=~w // Pr=~w~n',[Len,On,Pr]).


mk_ax(equivalentTo(X,Y),X,Y).
mk_ax(subClassOf(X,Y),X,Y).

simrun(N, Len) :-
        Init=[
             ],
        setof(X,between(1,N,X),Cs),
        findall(P-Ax,
                (   member(X,Cs),
                    member(Y,Cs),
                    mk_ax(Ax,X,Y),
                    random(P)),
                H),
        nodebug(search),
        search(Init, H, Sols, [max(1000)]),
        length(Sols, Len),
        Sols=[Sol|_],
        kb_prob(Sol,Pr),
        format('~n[[ SOLS N=~w NUM=~w MPE=~w~n',[N,Len,Pr]),
        maplist(write_kb,Sols),
        format('~n   SOLS N=~w NUM=~w MPE=~w~n]]~n~n',[N,Len,Pr]).



test(n2) :-
        simrun(2,Len),
        assertion( Len >  2).

test(n3) :-
        simrun(3,Len),
        assertion( Len >  2).

test(n4) :-
        simrun(4,Len),
        assertion( Len > 2 ).

test(n5) :-
        simrun(5,Len),
        assertion( Len > 2 ).


:- end_tests(scaling).
    
