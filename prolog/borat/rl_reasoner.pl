:- module(rl_reasoner,
          [saturate/2,
           abduce/3,
           abduce_all/3]).


/*

  DEDUCTIVE REASONING

*/
saturate(AxiomsIn, AxiomsOut) :-
        entails(AxiomsIn,A),
        !,
        saturate([A|AxiomsIn], AxiomsOut).
saturate(Axioms, Axioms).

%! entailed(+Axiom, +Axioms:list, ?AxiomEntailed) is nondet
%
% true if AxiomEntailed can be inferred from A+Axioms
entails(AxiomsIn, EntailedAxiom) :-
        rule(EntailedAxiom, [BodyAxiom|BodyAxioms]),
        member(BodyAxiom,AxiomsIn),
        % consider testing if already exists here, if already ground
        all_true(BodyAxioms, AxiomsIn),
        \+ member(EntailedAxiom, AxiomsIn).


all_true([],_).
all_true([A|As],AxiomsIn) :-
        ground_axiom(A,AxiomsIn),    % grounds Fact
        all_true(As,AxiomsIn).

%! abduce(+AxiomsIn:list, +AxiomsNew:list, ?AxiomExplained:list) is nondet
%
% finds axioms that are explained by AxiomsNew
abduce(AxiomsIn, AxiomsNew, AxiomExplained) :-
        select(AxiomExplained, AxiomsIn, AxiomsRest),
        \+ member(AxiomExplained, AxiomsNew),
        append(AxiomsNew, AxiomsRest, AxiomsIn2),
        saturate(AxiomsIn2, AxiomsOut),
        member(AxiomExplained, AxiomsOut).

abduce_all(AxiomsIn, AxiomsNew, AxiomsExplained) :-
        setof(A,abduce(AxiomsIn, AxiomsNew, A),AxiomsExplained).



% todo: other operations; this is the only native one required so far
ground_axiom(A\=B,_) :- A\=B.
ground_axiom(A,AxiomsIn) :- member(A,AxiomsIn).


rule(subClassOf(A,B),[subClassOf(A,Z),subClassOf(Z,B)]). % trans
rule(subClassOf(A,B),[equivalentTo(A,B)]).

rule(equivalentTo(A,B),[equivalentTo(B,A)]). % symmetry of equiv
rule(equivalentTo(A,B),[subClassOf(A,B),subClassOf(B,A)]). % anti-symm of sc
rule(equivalentTo(A,B),[equivalentTo(A,Z),equivalentTo(Z,B)]). % trans

rule(unsat(A),[subClassOf(A,B),subClassOf(A,C),disjointWith(B,C)]).
rule(unsat(A),[equivalentTo(A,B),in(A,S),in(B,S),A\=B,all_unique(S)]).
rule(unsat(Ax),[not(Ax),Ax]).





        
        
