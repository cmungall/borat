:- begin_tests(lexinf).

:- use_module(library(borat)).
:- use_module(library(borat/utils)).
:- use_module(library(borat/learner)).

showall(Insts) :-
        length(Insts,Num),
        format('~nLEARN: ~w~n',[Num]),
        G=learn(_,_,Insts,_,_),
        forall(G,
               writeln(G)).


test(learner) :-
        Insts=[
               instance(i0,a,yes),
               instance(i1,a,yes),
               instance(i2,a,yes),
               instance(i3,a,yes),
               instance(i4,a,yes),
               instance(i5,a,yes),
               instance(i6,a,yes),
               instance(i7,a,yes),
               instance(i8,a,yes),
               instance(i9,a,yes),
               instance(i10,b,no),
               instance(i11,b,no),
               instance(i12,b,no),
               instance(i13,b,no),
               instance(i14,b,no),
               instance(i15,b,no),
               instance(i16,b,no),
               instance(i17,b,no),
               instance(i18,b,no),
               instance(i19,b,no)
              ],
        showall(Insts),
        Insts2 = [instance(i0x,a,no),
                  instance(i1x,b,yes)],
        showall(Insts2),
        append(Insts,Insts2,Insts3),
        showall(Insts3),
        nl.

:- end_tests(lexinf).
    
