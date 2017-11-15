:- module(kboom,
          [extract_module/3,
           extract_module/4]).

extract_module(Nodes, Axioms, SubAxioms) :-
        extract_module(Nodes, Axioms, SubAxioms, []).
extract_module(Nodes, Axioms, SubAxioms, Opts) :-
        extract_module(Nodes, Axioms, [], SubAxioms, [], Opts).

extract_module([], Axioms, ModAxioms, ModAxioms2, _, _Opts) :-
        axioms_refnodes(ModAxioms,Ns,[]),
        findall(A,(member(A,Axioms),
                   all_axiom_refs_in(A,Ns)),
                ModAxioms1),
        ord_union(ModAxioms,ModAxioms1,ModAxioms2).
        
extract_module([N|Nodes], Axioms, MAs, Acc, VNodes, Opts) :-
        debug(extract,'~w from ~w',[N,Nodes]),
        setof(A,node_parent_axiom_member(N,A,Axioms),As),
        axioms_refnodes(As,NextNodes,[N|VNodes]),
        !,
        ord_union(As,MAs,NewMAs),
        ord_union(Nodes,NextNodes,Nodes2),
        ord_subtract(Nodes2,VNodes,NextNodes2),
        debug(extract,' Nextnodes = ~w Axs = ~w',[NextNodes, NewMAs]),
        extract_module(NextNodes2, Axioms, NewMAs, Acc, [N|VNodes], Opts).
extract_module([N|Nodes], Axioms, MAs, Acc, VNodes, Opts) :-
        debug(extract,'No new neighbors for ~w // ~w',[N,Nodes]),
        extract_module(Nodes, Axioms, MAs, Acc, VNodes, Opts).

node_parent_axiom_member(N,Pr-A,Axioms) :-
        member(Pr-A,Axioms),
        node_parent_axiom(N,A,_).
node_parent_axiom_member(N,A,Axioms) :-
        member(A,Axioms),
        A \= _-_,
        node_parent_axiom(N,A,_).

node_parent_axiom(N,subClassOf(N,_),_).
node_parent_axiom(N,properSubClassOf(N,_),_).
node_parent_axiom(N,equivalentTo(N,_),_).
node_parent_axiom(N,equivalentTo(_,N),_).

axiom_references(_-A,N) :- axiom_references(A,N).
axiom_references(T,N) :- T=..[_|Args],member(N,Args).

axioms_refnodes(As,Ns,VNs) :-
        setof(N2,A^(member(A,As),
                    axiom_references(A,N2),
                    \+member(N2,VNs)),Ns).

all_axiom_refs_in(A,Ns):-
        A=..[_|Args],
        forall(member(X,Args),
               member(X,Ns)).


        

