/*

  BORAT - Bayesian Ontology Reasoning Abduction Tool
  
*/

:- module(borat,
          [search/3,
           search/4,
           kb_search/2,
           kb_search/3,
           pquery/4,
           pquery/5,

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
kb_A(kb(X,_,_,_,_),X).

%! kb_H(+Kb,?PrAxiomPairs:list) is det
%
% list of pairs of Pr-Axiom from set of  hypothetical axioms
kb_H(kb(_,X,_,_,_),X).
kb_H_axioms(S,X) :- kb_H(S,H),findall(A,member(_-A,H),X).

kb_set_H(Kb,H,Kb2) :-
        Kb =.. [F,A,_|Args],
        Kb2 =.. [F,A,H|Args].



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

%! pquery(+Query:term, +Axioms:list, +PrAxioms:list, ?QueryProb:number, +Opts:list) is det
%
% finds the probability a query is true given a knowledge base of logical axioms (Axioms, aka A)
% and hypothetical axioms (PrAxioms, aka H), specified as Weight-Axiom or Prob-Axiom pairs
%
% Query is any boolean conjunction of axioms specified using standard prolog constructs (','/2, ';'/2).
%
% the space of possible consistent worlds is explored, 
pquery(Q,Axioms,PrAxioms,QueryProb,Opts) :-
        search(Axioms,PrAxioms,Sols,Opts),
        findall(QResult,
                (   member(Kb,Sols),
                    debug(query,'  World = ~w',[Kb]),
                    kb_satisfies_query(Q,Kb,QResult)),
                QResults),
        debug(xquery,'QueryResults=~w',[QResults]),
        QResults1 = [true-0, false-0 | QResults],
        aggregate(sum(P), member(true-P, QResults1), PrTrue),
        aggregate(sum(P), member(false-P, QResults1), PrFalse),
        debug(xquery,'Sum(PT)=~w Sum(PF)=~w',[PrTrue,PrFalse]),
        QueryProb is PrTrue / (PrFalse+PrTrue).

pquery(Q,Axioms,PrAxioms,QueryProb) :-
        pquery(Q,Axioms,PrAxioms,QueryProb,[]).

%! kb_satisfies_query(+Q,+Kb,?Result) is nondet
%
% Result = IsTrue-Prob
%
% nondet - unifies with two results, one for each value of IsTrue
kb_satisfies_query(Q,_,_) :-
        var(Q),
        throw(query_cannot_be_var(Q)).
kb_satisfies_query((X,Y),Kb,True-Pr) :-
        !,
        kb_satisfies_query(X,Kb,True1-Pr),
        kb_satisfies_query(Y,Kb,True2-Pr),
        eval_and(True1,True2,True).
kb_satisfies_query((X;Y),Kb,True-Pr) :-
        !,
        kb_satisfies_query(X,Kb,True1-Pr),
        kb_satisfies_query(Y,Kb,True2-Pr),
        eval_or(True1,True2,True).
kb_satisfies_query(not(Q),Kb,True-Pr) :-
        !,
        kb_satisfies_query(Q,Kb,PosTrue-Pr),
        eval_not(PosTrue,True).
kb_satisfies_query(\+(Q),Kb,Result) :-
        !,
        kb_satisfies_query(not(Q),Kb,Result).
kb_satisfies_query(Q,Kb,True-Pr) :-
        kb_A(Kb,A),
        kb_P(Kb,Pr),
        debug(query,' Pr(~q)=~w',[A,Pr]),
        (   member(Q,A)
        ->  True=true
        ;   True=false).

eval_not(true,false).
eval_not(false,true).

eval_and(true,true,true) :- !.
eval_and(_,_,false) :- !.

eval_or(false,false,false) :- !.
eval_or(_,_,true) :- !.



%! search(+LogicalAxioms:list, +PrAxioms:list, ?CandidateKbs:list) is det
% 
% Search solution space for most likely configuration of weighted axioms.
% 
% PrAxioms = [Pr1-Axiom1, Pr2-Axiom2, ...]
search(Axioms,PrAxioms,Sols2) :-
        search(Axioms,PrAxioms,Sols2,[]).
search(Axioms,PrAxioms,Sols,Opts) :-
        process_opts(Axioms,PrAxioms,Opts,Axioms2,PrAxioms2,Opts2),
        !,
        search(Axioms2,PrAxioms2,Sols,Opts2).
search(Axioms,PrAxioms,Sols2,Opts) :-
        Kb=kb(Axioms,PrAxioms,[],1,Axioms),
        preprocess_weights(Kb,Kb2,Opts),
        lsearch([Kb2], Sols, [], 1, Opts),
        !,
        length(Sols,NumSols),
        predsort(compare_kbs,Sols,Sols2),
        debug(search,'Solutions: ~w',[NumSols]).

kb_search(Kb,Sols2) :-
        kb_search(Kb,Sols2,[]).
kb_search(Kb,Sols2,Opts) :-
        kb_A(Kb,Axioms),
        kb_H(Kb,PrAxioms),
        search(Axioms,PrAxioms,Sols2,Opts).

compare_kbs(Order,Kb1,Kb2) :-
        kb_P(Kb1,P1),
        kb_P(Kb2,P2),
        compare(Order,P2-Kb2,P1-Kb1).

% succeeds if an option can be processed, creating
% a new A/H set

process_opts(Axioms,PrAxioms,Opts,Axioms2,PrAxioms,Opts2) :-
        % local unique name assumption
        select(una(local),Opts,Opts2),
        setof(all_unique(Src),C^member(in(C,Src),Axioms),NewAxioms),
        !,
        append(Axioms,NewAxioms,Axioms2).

%! preprocess_weights(+Kb,?Kb2,+Opts:list) is det
%
% input weighted axiom can either be Prob-Axiom or w(Weight)-Axiom
%
% here we normalize to probabilities, after first
% summing weights for the same axiom
preprocess_weights(Kb, Kb2, _Opts) :-
        kb_H(Kb,H),
        maplist(prob_to_weight,H,H2),
        sumweights(H2,H3),
        maplist(weight_to_prob,H3,H4),
        kb_set_H(Kb,H4,Kb2).

% PAs can include the same axiom with multiple weights
% PAs2 is guaranteed to have unique axioms with weights for same axiom summed
sumweights(PAs,PAs2) :-
        findall(w(SumW)-A,
                aggregate(sum(W),member(w(W)-A, PAs),SumW),
                PAs2).

% logit
prob_to_weight(PA, PA) :-
        % already a weight
        PA=w(_)-_,
        !.
prob_to_weight(P-A, w(W)-A) :-
        W is log(P/(1-P))/log(2),
        !.

% inverse logit
weight_to_prob(w(W)-A, P-A) :-
        !,
        P is 1/(1+2**(-W)).
weight_to_prob(PA,PA).
        

%! lsearch(+KbStack:list, ?SolvledKbs:list, +Visited:list, +Counter:int) is det
%
% depth first search of solution space

lsearch([], [], _, Counter, _Opts) :-
        % BASE CASE 1
        debug(search,'All solutions explored after ~w iterations',[Counter]).
lsearch([_S|_], [], _, Counter, Opts) :-
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
        debug(xsearch,'ZERO-PROB DEAD-END: ~w',[S]),
        kb_P(S,0),
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
        adj(S, S2, VList, VList2, Opts),
        !,

        % depth-first
        CounterPlus1 is Counter+1,
        lsearch([S2,S|Sols], TerminalSols, VList2, CounterPlus1, Opts).

lsearch([_|Sols], TerminalSols, VList, Counter, Opts) :-
        lsearch(Sols, TerminalSols, VList, Counter, Opts).

%% adj(+Kb, ?NextKb, +VList:list, ?VlistUpdated) is semidet
%
% find adjacent/next node
%
% currently just selects next-most probable independent of adjacency
adj(S, S2, VList, [Sig2|VList], Opts) :-
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
        debug(xsearch,'Choices = ~w',[PAs]),
        reverse(PAs,[Pr-A|_]),
        saturate([A|Axioms], AxiomsInf),
        PrAxiomsOn2 = [Pr-A|PrAxiomsOn],
        add_axiom(A,Sig,Sig2),
        debug(search,'EXT = ~w + ~w ==> ~w',[A,Sig,Sig2]),
        S2 = kb(AxiomsInf, PrAxioms, PrAxiomsOn2, Prob, OrigAxioms),
        % this grounds Prob
        calc_kb_prob(S2, A, Opts),
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

%! calc_kb_prob(+Kb, +NextAxiom, +Opts:list) is det
%! 
% calculates prob and unifies free variable
calc_kb_prob(S, _A, _Opts) :-
        kb_A(S,Axioms),
        % Pr=0 if ontology is incoherent
        member(unsat(_), Axioms),
        debug(xsearch,'UNSAT = ~w',[Axioms]),
        kb_P(S,0),
        !.


calc_kb_prob(S, NextAxiom , Opts) :-
        kb_S(S,PrAxiomsOn),
        % product of all probabilities
        aproduct(PrAxiomsOn, 1, ProbProduct),
        explanatory_power(S, NextAxiom,Opts,Power),
        % Crude: noisy-OR of two possible outcomes
        % todo: this should be conditioned on the priors of supporting axioms
        Prob is 1-( (1/(2**Power)) * (1-ProbProduct) ),
        kb_P(S,Prob),
        !.

%! explanatory_power(+Kb, +NextAxiom, +Opts:list, ?Power:int) is det
%
% calculate explanatory power of an axiom
% see explain_test for example.
explanatory_power(_, _, Opts, 0) :-
        \+ member(use_explain(true),Opts),
        !.
explanatory_power(S, NextAxiom, _Opts, Power) :-
        NextAxiom\=in(_),
        NextAxiom\=not(_),
        kb_A_orig(S,OrigAxioms),
        kb_H_axioms(S,OnAxioms),
        select(NextAxiom, OnAxioms, OnAxiomsWithout),
        append(OrigAxioms,OnAxiomsWithout,BaseAxioms),
        setof(EA,get_expl(BaseAxioms, NextAxiom, EA),EAs),
        !,
        length(EAs,Power).
explanatory_power(_S, _NextAxiom, _Opts, 0).

% TODO: consider backwards chaining
get_expl(Axioms,NewA,EA) :-
        select(EA,Axioms,Axioms2),
        saturate(Axioms2,AxiomsInfWithout),
        \+ member(EA,AxiomsInfWithout),
        saturate([NewA|AxiomsInfWithout],AxiomsInf),
        member(EA,AxiomsInf).

% adds an axiom to an axiom signature
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
