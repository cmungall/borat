/*

  BORAT - Bayesian Ontology Reasoning Abduction Tool
  
*/

:- module(borat,
          [search/3,
           search/4,

           kb_A/2,
           kb_H/2,
           kb_S/2,
           kb_P/2
           ]).

:- use_module(borat/rl_reasoner).

/*
  util predicates
*/

%! kb_A(+Kb,?LogicalAxioms:list) is det
%
% all logical axioms in KB
kb_A(kb(X,_,_,_),X).

%! kb_H(+Kb,?PrAxiomPairs:list) is det
%
% list of pairs of Pr-Axiom from set of  hypothetical axioms
kb_H(kb(_,X,_,_,_),X).

%! kb_S(+Kb,?AxiomsInSolution:list) is det
%
% all axioms selected as true in current solution
kb_S(kb(_,_,X,_,_),X).

%! kb_P(+Kb,?Prob:number) is det
%
% probability of KB
kb_P(kb(_,_,_,X,_),X).

%! kb_A_orig(+Kb,?LogicalAxioms:list) is det
%
% original/seed set of axioms
kb_A_orig(kb(_,_,_,_,X),X).

        
%! search(+LogicalAxioms:list, +PrAxioms:list, ?CandidateKbs:list) is det
% 
% Search solution space for most likely configuration of weighted axioms.
% 
% PrAxioms = [Pr1-Axiom1, Pr2-Axiom2, ...]
search(Axioms,PrAxioms,Sols2) :-
        search(Axioms,PrAxioms,Sols2,[]).
search(Axioms,PrAxioms,Sols2,Opts) :-
        lsearch([kb(Axioms,PrAxioms,[],1,Axioms)], Sols, [], 1, Opts),
        !,
        length(Sols,NumSols),
        predsort(compare_kbs,Sols,Sols2),
        debug(search,'Solutions: ~w',[NumSols]).

compare_kbs(Order,Kb1,Kb2) :-
        kb_P(Kb1,P1),
        kb_P(Kb2,P2),
        compare(Order,P2-Kb2,P1-Kb1).

%! lsearch(+KbStack:list, ?SolvledKbs:list, +Visited:list, +Counter:int) is det
%
% depth first search of solution space

lsearch([], [], _, Counter, _Opts) :-
        % BASE CASE 1
        debug(search,'All solutions explored after ~w iterations',[Counter]).
lsearch([S|_], [], _, Counter, Opts) :-
        % BASE CASE 2
        1 is Counter mod 100,

        member(max(Max),Opts),
        Counter > Max,
        !,
        debug(search,'Stopping after ~w iterations',[Counter]).

lsearch([S|Sols], TerminalSols, VList, Counter, Opts) :-
        % DEAD-END
        % A zero-probability solution is not explored further
        % and not added to the list of candidate solutions
        kb_P(S,Prob),
        Prob = 0,
        !,
        lsearch(Sols, TerminalSols, VList, Counter, Opts).

lsearch([S|SRest], [S|TerminalSols], VList, Counter, Opts) :-
        % no further choices - terminal node, add to candidate solutions stack
        kb_H(S,PrAxioms),
        kb_S(S,PrAxiomsOn),
        debug(xsearch,'Testing for terminal ~w len= ~w',[PrAxioms,PrAxiomsOn]),
        length(PrAxioms,Len),
        length(PrAxiomsOn,Len),
        !,
        CounterPlus1 is Counter+1,
        lsearch(SRest, TerminalSols, VList, CounterPlus1, Opts).

lsearch([S|Sols], TerminalSols, VList, Counter, Opts) :-
        % non-terminal: extend node

        % get adjacent solution
        adj(S, S2, VList, VList2),
        !,

        % depth-first
        CounterPlus1 is Counter+1,
        lsearch([S2,S|Sols], TerminalSols, VList2, CounterPlus1, Opts).

lsearch([_|Sols], TerminalSols, VList, Counter, Opts) :-
        lsearch(Sols, TerminalSols, VList, Counter, Opts).

%% adj(+Kb, ?Kb, +VList:list, ?VlistUpdated) is semidet
%
% find adjacent/next node
%
% currently just selects next-most probable independent of adjacency
adj(S, S2, VList, [Sig2|VList]) :-
        debug(xsearch,'Choosing from ~w',[S]),
        S = kb(Axioms, PrAxioms, PrAxiomsOn, _, OrigAxioms),
        pr_axioms_ids(PrAxiomsOn, Sig),
        findall(PA, (choose(PrAxioms,Pr,A,PrAxiomsOn,Axioms),
                     PA = Pr-A,
                     add_axiom(A,Sig,Sig2),
                     \+ member(Sig2, VList)
                  ),
                PAs),
        PAs\=[],
        !,
        debug(xsearch,'Coices = ~w',[PAs]),
        reverse(PAs,[Pr-A|_]),
        saturate([A|Axioms], AxiomsInf),
        PrAxiomsOn2 = [Pr-A|PrAxiomsOn],
        add_axiom(A,Sig,Sig2),
        debug(search,'EXT = ~w + ~w ==> ~w',[A,Sig,Sig2]),
        %calc_prob(AxiomsInf, PrAxiomsOn2, Prob),
        S2 = kb(AxiomsInf, PrAxioms, PrAxiomsOn2, Prob, OrigAxioms),
        % this grounds Prob
        calc_kb_prob(S2),
        debug(xsearch,'PROB = ~w',[Prob]).



%! choose(+PrAxioms:list, ?Pr, ?A, +PrAxiomsOn:list, +Axioms:list) is nondet
%
% select an axiom that is in H but not in the current A
choose(PrAxioms,Pr,A,PrAxiomsOn,Axioms) :-
        member(PrPos-PosA,PrAxioms),
        % TODO: investigate if more efficient to keep stack of remaining items in H
        \+member(_-PosA, PrAxiomsOn),
        \+member(_-not(PosA), PrAxiomsOn),
        \+member(PosA, Axioms),
        \+member(not(PosA), Axioms),
        (   Pr=PrPos,
            A=PosA
        ;   Pr is 1-PrPos,
            A=not(PosA)).

% calculates prob and unifies free variable
calc_kb_prob(S) :-
        kb_A(S,Axioms),
        % Pr=0 if ontology is incoherent
        member(unsat(_), Axioms),
        kb_P(S,0),
        !.
calc_kb_prob(S) :-
        kb_S(S,PrAxiomsOn),
        % product of all probabilities
        aproduct(PrAxiomsOn, 1, Prob),
        kb_P(S,Prob),
        !.

% for signature
add_axiom(A,Sig,Sig2) :-
        axiom_id(A,A_id),
        ord_union([A_id],Sig,Sig2).
        
% For efficiency we translate axioms to Ids
:- dynamic axiom_id_fact/2.
axiom_id(A,Id) :-  axiom_id_fact(A,Id),!.
axiom_id(A,Id) :-  gensym(a,Id),assert(axiom_id_fact(A,Id)),!.

% todo - use bitwise ops
pr_axioms_ids(PrAxioms,Ids) :- setof(Id,Pr^A^(member(Pr-A,PrAxioms),axiom_id(A,Id)),Ids),!.
pr_axioms_ids(_,[]).


aproduct([],P,P).
aproduct([Pr-_|L],P1,POut) :-
        P2 is Pr*P1,
        aproduct(L,P2,POut).
