% -*- Mode: prolog -*-
% README.txt

:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.
:- dynamic heap/2.
:- dynamic heap_entry/4.
:- dynamic previous/3.
:- dynamic save/2.
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_graph(G) :- graph(G), !.
new_graph(G) :- assert(graph(G)), !.

delete_graph(G):- findall(arc(G, V, F, P), arc(G, V, F, P), Al),retract_list(Al),
                 findall(vertex(G, N), vertex(G, N), Nl), retract_list(Nl),
                 findall(vertex_key(G, Vk, W), vertex_key(G, Vk, W), Vkl),
                 retract_list(Vkl),
                 findall(vertex_previous(G, Vp, U), vertex_key(G, Vp, U), Vpl),
                 retract_list(Vpl) .

retract_list([]).
retract_list([X | Xs]):- retract(X), retract_list(Xs), !.

new_vertex(G, V) :- vertex(G, V), !.
new_vertex(G, V) :- assert(vertex(G, V)), !.

graph_vertices(G, Vs) :- findall(V, vertex(G, V), Vs).

list_vertices(G) :- graph(G), listing(vertex(G, _)).


new_arc(_, U, U, _) :- !.
new_arc(G, U, V, Weight) :- arc(G, U, V, Weight), !.
new_arc(G, U, V, Weight) :- arc(G, U, V, WeightOld),
			    retract(arc(G, U, V, WeightOld)),
			    assert(arc(G, U, V, Weight)), !.

new_arc(G, U, V, Weight) :- assert(arc(G, U, V, Weight)), !.
new_arc(G, U, V) :- new_arc(G, U, V, 1), !.


graph_arcs(G, Es) :- findall(arc(G, U, V, D), arc(G, U, V, D), Es).


vertex_neighbors(G, V, Nst) :- findall(arc(G, V, F, D),
			               arc(G, V, F, D), Ns),
			       findall(arc(G, A, V, D),
				       arc(G, A, V, D), Nsr),
			       append(Ns, Nsr, Nst).

list_arcs(G) :- graph(G), listing(arc(G, _, _, _)).

list_graph(G) :- graph(G), listing(vertex(G, _)), listing(arc(G, _, _, _)).

adjs(G, V, Vsd) :- vertex(G, V), findall(F, arc(G, V, F, D), Vs),
			         findall(A, arc(G, A, V, D), Vsr),
			         append(Vs, Vsr, Vsd).


read_graph(G,FileName):- new_graph(G), csv_read_file(FileName,Output,[functor(table),
                        arity(3),separator(0' )]), save(G,Output),!.

save(_,[]).
save(G,[T|Ts]):- arg(1,T,X),
    arg(2,T,Y),
    arg(3,T,Z),
    new_vertex(G,X),
    new_vertex(G,Y),
    new_arc(G,X,Y,Z),save(G, Ts),!.

write_graph(G, FileName) :- write_graph(G, FileName, 'graph').

write_graph(G,FileName,'graph'):- graph_arcs(G, X), csv_write_file(FileName,X).

write_graph(graph_arcs(G, X), FileName,'edge'):- graph_arcs(G, X), csv_write_file(FileName,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% minHeap


new_heap(H) :- heap(H, _S), !.
new_heap(H) :- assert(heap(H, 0)), !.

new_heap(H, S) :- heap(H, S), !.
new_heap(H, S) :- assert(heap(H, S)), !.


delete_heap(H) :-  retractall(heap_entry(H, _, _, _)), retract(heap(H, _)).

heap_has_size(H, S) :- heap(H, S).

heap_empty(H) :- heap_has_size(H, 0).

heap_not_empty(H) :- not(heap_empty(H)).

list_heap(H) :- listing(heap_entry(H, _, _, _)).

heapify(H, Ip) :- heap_has_size(H, S), X is (Ip*2), X > S, !.

heapify(H, Ip) :- heap_entry(H, Ip, Kp, _), Il is (Ip * 2), Ir is (Il + 1),
                  heap_entry(H, Il, Kl, _), heap_entry(H, Ir, Kr, _),
		  Kp =< Kl, Kp =< Kr, !.

heapify(H, Ip) :- heap_entry(H, Ip, Kp, Vp),
		  Il is (Ip * 2), Ir is (Il + 1),
                  heap_entry(H, Il, Kl, Vl), heap_entry(H, Ir, Kr, Vr),
		  (Kl < Kp, Kl < Kr, retract(heap_entry(H, Ip, Kp, Vp)),
		     retract(heap_entry(H, Il, Kl, Vl)),
		     assert(heap_entry(H, Il, Kp, Vp)),
		     assert(heap_entry(H, Ip, Kl, Vl)), heapify(H, Il),!;
                  retract(heap_entry(H, Ip, Kp, Vp)),
		     retract(heap_entry(H, Ir, Kr, Vr)),
		     assert(heap_entry(H, Ir, Kp, Vp)),
		     assert(heap_entry(H, Ip, Kr, Vr)), heapify(H, Ir), !).


heapify(H, Ip) :- heap_entry(H, Ip, Kp, _), Il is (Ip *2),
		  heap_entry(H, Il, Kl, _), Kp =< Kl, !.

heapify(H, Ip) :- heap_entry(H, Ip, Kp, Vp), Il is (Ip * 2),
		  heap_entry(H, Il, Kl, Vl),
		  retract(heap_entry(H, Ip, Kp, Vp)),
		  retract(heap_entry(H, Il, Kl, Vl)),
		  assert(heap_entry(H, Il, Kp, Vp)),
		  assert(heap_entry(H, Ip, Kl, Vl)),
		  heapify(H, Il), !.

heapify(H, Ip) :- heap_entry(H, Ip, Kp, _), Ir is ((Ip *2)+1),
		  heap_entry(H, Ir, Kr, _), Kp =< Kr, !.


heapify(H, Ip) :- heap_entry(H, Ip, Kp, Vp), Ir is ((Ip * 2) + 1),
		  heap_entry(H, Ir, Kr, Vr),
                  retract(heap_entry(H, Ip, Kp, Vp)),
		  retract(heap_entry(H, Ir, Kr, Vr)),
		  assert(heap_entry(H, Ir, Kp, Vp)),
		  assert(heap_entry(H, Ip, Kr, Vr)),
		  heapify(H, Ir), !.

build_heap(_,0).

build_heap(H,1):- heapify(H,1),!.

build_heap(H,L):- heapify(H,L),L1 is L-1, build_heap(H,L1).


parent_swap(_, 1).

parent_swap(H, If) :- heap_entry(H, If, Kf, _), Ip is floor(If/2),
		      heap_entry(H, Ip, Kp, _), Kf >= Kp, !.

parent_swap(H, If) :- heap_entry(H, If, Kf, Vf), Ip is floor(If/2),
		      heap_entry(H, Ip, Kp, Vp), Kf < Kp,
                      retract(heap_entry(H, Ip, Kp, Vp)),
		      retract(heap_entry(H, If, Kf, Vf)),
		      assert(heap_entry(H, If, Kp, Vp)),
		      assert(heap_entry(H, Ip, Kf, Vf)),
		      parent_swap(H, Ip), !.

modify_key(H, NewKey, OldKey, V) :- heap_entry(H, I, OldKey, V),
				    retract(heap_entry(H, I, OldKey, V)),
				    assert(heap_entry(H, I, NewKey, V )),
                                    parent_swap(H, I),
				    heap_entry(H, In, NewKey, V),
				    heapify(H, In), !.


heap_insert(H, K, V) :- heap(H, S), retract(heap(H, S)), Som is S + 1,
			new_heap(H, Som), assert(heap_entry(H, Som, K, V)),
			Som2 is floor(Som/2),
			build_heap(H, Som2).


heap_insert_non_ordinata(H, K, V) :- heap(H, S), retract(heap(H, S)),
				     Som is S + 1, new_heap(H, Som),
				     assert(heap_entry(H, Som, K, V)).

heap_head(H, K, V):- heap_entry(H, 1, K, V).


heap_extract(H, K, V) :-  heap_head(H, K, V), heap_has_size(H, L), L = 1,
			  retract(heap_entry(H,1, K, V)), retract(heap(H, L)),
			  assert(heap(H, 0)), !. %caso1

heap_extract(H, K, V) :-  heap_head(H, K, V), heap_has_size(H, L),
			  heap_entry(H, L, Lk, Lv),
			  retract(heap_entry(H, L, Lk, Lv)),
			  retract(heap_entry(H,1, K, V)),
			  assert(heap_entry(H, 1, Lk, Lv)),
                          retract(heap(H, L)), Ln is L-1,
			  assert(heap(H, Ln)), heapify(H, 1). %caso2


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% MST

mst_inizialize(G, H):- graph_vertices(G, Es), Es = [V| Vs],
                       assert(vertex_key(G, V, 0)),
		       mst_heap_inizialize(G , H, Vs).


mst_heap_inizialize(_, _, []) :- !.

mst_heap_inizialize(G, H, [Vertex | Vertexs]) :- heap_insert_non_ordinata(H, inf, Vertex),
						 mst_heap_inizialize(G, H, Vertexs).

vertex_neighbors_no_mst(G, V, Arcs):- findall(arc(G, V, F, P),
                                       (arc(G, V, F, P),
			              not(vertex_key(G, F, _))), Ns),
		                      findall(arc(G, A, V, P2),
                                       (arc(G, A, V, P2),
                                        not(vertex_key(G, A, _))), Nsr),
		                      append(Ns, Nsr, Arcs).

mst_heap_insert(H, New_arc):- (New_arc = arc(_, _, Arrivo, Peso),
			         heap_entry(H, _, Old_peso, Arrivo),
				 Peso < Old_peso,
				 modify_key(H, Peso, Old_peso, Arrivo), !;
                               New_arc = arc(_, Arrivo, _, Peso),
				 heap_entry(H, _, Old_peso, Arrivo),
				 Peso < Old_peso,
				 modify_key(H, Peso, Old_peso, Arrivo)), !.

mst_heap_insert(H, New_arc):- (New_arc = arc(_, _, Arrivo, Peso),
			         heap_entry(H, _, Old_peso, Arrivo), Peso >= Old_peso, !;
                               New_arc = arc(_, Arrivo, _, Peso),
				 heap_entry(H, _, Old_peso, Arrivo), Peso >= Old_peso), !.

insert_arcs_in_heap([], _):- !.

insert_arcs_in_heap([Arc | Arcs], H):- mst_heap_insert(H, Arc),
				      insert_arcs_in_heap(Arcs, H).


heap_update(G, Vertex, H):- vertex_neighbors_no_mst(G, Vertex, Arcs),
			    insert_arcs_in_heap(Arcs, H), !.


mst_insert(_, H, _):- heap_has_size(H, X), X = 0, !.

mst_insert(G, H, [Vertex |_]):- heap_update(G, Vertex, H), heap_head(H, inf, _), !.

mst_insert(G, H, Mst_Vs):- heap_extract(H, Peso, Vertex), assert(vertex_key(G, Vertex, Peso)),
			  (arc(G, Parent, Vertex, Peso) ,
                              assert(vertex_previous(G, Vertex, Parent)),
			      mst_insert(G, H, [Vertex | Mst_Vs]), !;
			   arc(G, Vertex, Parent, Peso),
			      assert(vertex_previous(G, Vertex, Parent)),
			      mst_insert(G, H,  [Vertex | Mst_Vs]) ), !.


mst_get_arc(G, N, Arc):- vertex_previous(G, N, U),
			 vertex_key(G, N, P), Arc = arc(G, U, N, P).


mst_figli(_, [], [] ):- ! .

mst_figli(G, [F | Fs], L):- mst_output(G, F, Lo), append(Lo, Ln, L),
			    mst_figli(G, Fs, Ln), ! .

mst_figli(G, [F | Fs], [Lo | Ln]):- mst_output(G, F, Lo) ,
				    mst_figli(G, Fs, Ln), ! .


mst_output(G, P, Arc):- not(vertex_previous(G, _, P)),
		        mst_get_arc(G, P, Arc),!.

mst_output(G, P, [Arc | Preorder]):- findall(F, vertex_previous(G, F, P), Lf),
				     mst_get_arc(G, P, Arc), sort(Lf, Lfs),
				     mst_figli(G, Lfs, Preorder) , !.


mst_prim(G, Source):-  read_graph(G, Source), new_heap(H), mst_inizialize(G, H),
		       vertex_key(G, V, 0), mst_insert(G, H, [V]).

mst_get(G, _, Preorder):- vertex_key(G, S, 0), findall(F, vertex_previous(G, F, S), Lf),
			  sort(Lf, Lfs), mst_figli(G, Lfs, Preorder).

%end of file -*- README.txt
