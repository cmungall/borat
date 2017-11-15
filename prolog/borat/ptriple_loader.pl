/*

  Loads triples or quads from an RDF format and interprets as a probabilistic knowledgebase.

  Logical axioms are encoded in the standard way.

  Probabilistic axioms are encoded in one of two ways:

   - each hypothetical axiom in a named graph G, with <G :prob "NUM"^^xsd:float>
   - each hypothetical axiom has axiom annotations :prob "NUM"^^xsd:float

  Here :prob is a predicate that relates the statement/graph to a probability value
  currently hardcoded to http://semanticscience.org/resource/SIO_000638

  See trig_test for details
  
  TODO: allow weights as well as probs

*/

:- module(ptriple_loader,
          [kb_from_rdf/1,
           kb_from_rdf_file/2]).

:- use_module(library(borat)).
:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_turtle)).

:- rdf_meta a2t(-,r).

a2t( subClassOf(A,B), rdf(A,rdfs:subClassOf,B,_G) ).
a2t( equivalentTo(A,B), rdf(A,owl:equivalentClass,B,_G) ).
a2t( disjointWith(A,B), rdf(A,owl:disjointWith,B,_G) ). % TODO
a2t( in(A,B), rdf(A,rdfs:isDefinedBy,B,_G) ).

%! kb_from_rdf(?Kb) is det
%
% initialize a KB from currently loaded rdf_graph
kb_from_rdf(Kb) :-
        findall(A,axiom(A),As),
        debug(rdf,'A=~w',[As]),
        kb_A(Kb,As),
        findall(W-A,weighted_axiom(A,W),H),
        debug(rdf,'H=~w',[H]),
        kb_H(Kb,H).

kb_from_rdf_file(File,Kb) :-
        rdf_load(File),
        kb_from_rdf(Kb).

axiom(A) :-
        a2t(A, T),
        debug(rdf,'Q: ~w',[T]),
        T,
        debug(rdf,'A: ~w',[T]),
        \+ triple_weight(T,_).
weighted_axiom(A,W) :-
        a2t(A, T),
        T,
        triple_weight(T,W).

weight_pred('http://semanticscience.org/resource/SIO_000638').


triple_weight(T,W) :-
        weight_pred(P),
        T=rdf(_,_,_,G),
        rdf(G,P,W^^_).
triple_weight(T,W) :-
        weight_pred(P),
        T=rdf(S,P,O,_),
        rdf(A,owl:annotatedSource,S),
        rdf(A,owl:annotatedProperty,P),
        rdf(A,owl:annotatedTarget,O),
        rdf(A,P,W^^_).





        

