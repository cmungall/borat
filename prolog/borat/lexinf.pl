:- module(lexinf,
          [ann/3,
           ann/4,
           ann/6,

           lmatch/2,
           lmatch/8,

           predict_relation/3,
           training_instance/2,
           training_instance/3
           ]).

:- use_module(library(porter_stem)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(tabling)).

:- use_module(learner).

:- rdf_meta pmap(-,r).
:- rdf_register_ns(oio,'http://www.geneontology.org/formats/oboInOwl#').


% TODO: make easier to extend/plugin
pmap(label, rdfs:label).
pmap(related, oio:hasRelatedSynonym).
pmap(exact, oio:hasExactSynonym).
pmap(broad, oio:hasBroadSynonym).
pmap(narrow, oio:hasNarrowSynonym).

:- table ann/3, ann/4, ann/6.
     
ann(X,P,V) :-
        ann(X,P,V,_).
ann(X,P,V,T) :-
        pmap(P,P1),
        T=rdf(X,P1,O),
        O=S^^_,
        T,
        atom_string(V,S).

ann(X,P,V2,T,F,V) :-
        ann(X,P,V,T),
        mutate(F,P,V,V2).
ann(X,P,V,T,null,V) :-
        ann(X,P,V,T).

mutate(stem,_,V,V2) :-
        custom_porter_stem(V,V2).
mutate(downcase,_,V,V2) :-
        downcase_atom(V,V2).

custom_porter_stem(T,S) :-
        atom_concat(X,eous,T),
        atom_concat(X,eus,T2),
        !,
        porter_stem(T2,S).
custom_porter_stem(T,S) :-
        porter_stem(T,S).

:- table lmatch/2.
lmatch(C1,C2,P1,P2,V,T1,T2,F) :-
        ann(C1,P1,V,T1,F,_),
        ann(C2,P2,V,T2,F,_),
        C1\=C2.
lmatch(C1,C2) :- lmatch(C1,C2,_,_,_,_,_,_).


triple_pv(rdf(S,P,O),AP,AV) :-
        rdf(Ax,owl:annotatedSource,S),
        rdf(Ax,owl:annotatedProperty,P),
        rdf(Ax,owl:annotatedTarget,O),
        rdf(Ax,AP,AV).

:- table training_instance/3.

training_instance(Rel,M) :-
        training_instance(Rel,M,_).
training_instance(Rel,M,C1-C2) :-
        lmatch(C1,C2,P1,P2,_V,_T1,_T2,F),
        M=m(O1,O2,P1,P2,F),
        belongs(C1,O1),
        belongs(C2,O2),
        rel(C1,C2,Rel).

predict_relation(Rel,M,Pr) :-
        setof(instance(I,M,Rel),training_instance(Rel,M,I),Insts),
        learn(Rel,M,Insts,Pr).


xxxsetof_features(Ms) :-
        setof(M,Rel^training_instance(Rel,M),Ms).
xxxlearn(Rel,M,Pr) :-
        aggregate(count,I^M1^R1^training_instance(R1,M1,I),NumInstances),
        rel(Rel),        
        aggregate(count,I^M1^training_instance(Rel,M1,I),NumWithRel),
        PrBackground is NumWithRel / NumInstances,
        setof_features(Ms),
        member(M,Ms),
        aggregate(count,I^R1^training_instance(R1,M,I),Num),
        aggregate(count,I^training_instance(Rel,M,I),NumPos),
        % todo - two-sided
        p_value_by_hypergeometric(NumPos,Num, NumWithRel,NumInstances,PVal),
        Pr1 is NumPos/Num,
        Pr is (PVal * PrBackground) + ((1-PVal)*Pr1).

        

rel(equivalentTo).
rel(subClassOf).
rel(superClassOf).

rel(C1,C2,equivalentTo) :-
        owl_equivalent_to(C1,C2),
        !.
rel(C1,C2,subClassOf) :-
        owl_subclass_of(C1,C2),
        !.
rel(C1,C2,superClassOf) :-
        owl_subclass_of(C2,C1),
        !.
rel(C1,C2,relatedTo) :-
        owl_subclass_of(C1,Z),        
        owl_subclass_of(C2,Z).

:- table owl_subclass_of/2.
owl_subclass_of(C1,C2) :-
        owl_equivalent_to(C1,C1x),
        rdf(C1x,rdfs:subClassOf,C2x),
        owl_equivalent_to(C2x,C2).
owl_subclass_of(C1,C2) :-
        owl_subclass_of(C1,X),
        owl_subclass_of(X,C2).

:- table owl_equivalent_to/2.
owl_equivalent_to(C,C).

owl_equivalent_to(C1,C2) :-
        owl_equivalent_to(C2,C1).

owl_equivalent_to(C1,C2) :-
        owl_equivalent_to(C1,X),
        owl_equivalent_to(X,C2).

owl_equivalent_to(C1,C2) :-
        rdf(C1,owl:equivalentClass,C2).
owl_equivalent_to(C1,C2) :-
        rdf(C1,owl:equivalentClass,C2).




% TODO: adapt for other ontology URI forms
belongs(C,O) :-
        concat_atom([O,_],'_',C).


        

    
