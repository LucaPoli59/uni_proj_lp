% -*- Mode: Prolog -*-
% README.txt


%---------%---------%---------%---------%---------%---------%---------%
% INTRODUZIONE %

Nome progetto: mst.pl

Obbiettivo: Lo scopo di questo progetto è di implementare l’algoritmo di Prim  per la soluzione
del problema MST per grafi non-diretti e connessi con pesi non negativi. Per procedere all’implementazione 
di questi algoritmi è necessario produrre un’implementazione di un MINHEAP.


Conoscenze necessarie per la comprensione del progetto:
Conoscenze base di programmazione in linguaggio Prolog
Conoscenze grafi, heap, heapsort


%---------%---------%---------%---------%---------%---------%---------%


Progetto scolastico livello universitario a cura di:

Luca Poli 
Ismaele Nachit Le Voci
Riccardo Moschi


%---------%---------%---------%---------%---------%---------%---------%


Linguaggio: SWI-Prolog 8.2.1
Potrebbe funzionare con altre versioni di Prolog successive alla 8.0 in quanto
in questa è stato implementata il predicato inf/0 che restituisce la 
rappresentazione di infinito positivo.


%---------%---------%---------%---------%---------%---------%---------%

Legenda:
La simbologia "%% Predicati extra aggiunti da noi %%" prima di un predicato indica che quel 
predicato non era stato espressamente richiesto dal professore ma necessario 
per il funzionamento del tutto.


%---------%---------%---------%---------%---------%---------%---------%

Istruzioni:
Per procedere all'utilizzo dell'applicativo con grafi molto grandi è necessario utilizzare un file di tipo .csv . 
in questo modo è possibile utilizzare l'algoritmo anche con grafi molto grandi, senza dover aggiungere a mano ogni vertice e arco singolarmente.
Il file .csv deve rispettare questa determinata formattazione:

vertice vertice arco

Ad esempio:

a b 10
a c 20

%---------%---------%---------%---------%---------%---------%---------%

Spiegazione dei predicati utilizzati:

% INTERFACCIA PROLOG PER LA MANIPOLAZIONE DEI GRAFI %


:- new_graph(G).
Questo predicato inserisce un nuovo grafo nella base-dati Prolog. 


:- delete_graph(G).
Rimuove tutto il grafo (vertici e archi inclusi) dalla base-dati Prolog.


:- new_vertex(G, V).
Aggiunge il vertice V nella base-dati Prolog.


:- graph_vertices (G, Vs).
Questo predicato è vero quando Vs è una lista contenente tutti i vertici di G.
 


:- list_vertices(G).
Questo predicato stampa alla console dell’interprete Prolog una lista dei 
vertici del grafo G.


:- new_arc(G, U, V, Weight).
Aggiunge un arco del grafo G alla base dati Prolog. 

:- graph_arcs(G, Es).
Questo predicato è vero quando Es è una lista di tutti gli archi presenti in G.


:- vertex_neighbors(G, V, Ns).
Questo predicato è vero quando V è un vertice di G e Ns è una lista contenente
gli archi, arc(G, V, N, W) e arc(G, N, V, W), che portano ai vertici N
immediatamente raggiungibili da V .


:- list_arcs(G).
Questo predicato stampa alla console dell’interprete Prolog una lista degli 
archi del grafo G.


:- list_graph(G).
Questo predicato stampa alla console dell’interprete Prolog una lista dei 
vertici e degli archi del grafo G.

:- adjs(G, V, Vs).
Questo predicato è vero quando Vs è una lista che contiene tutti i vertici
adiacenti a V.

:- write_graph(G, Filename).
Questo predicato scrive su file il grafo G

:- save(G, [T | Ts]).
Questo predicato prende ogni record contenuto nella lista Ts, e va a inserire 
i dati contenuti al suo interno

%% Predicati extra aggiunti da noi %%

:- retract_list([X | Xs]).
Questo predicato effettua la retract di tutti gli elementi di una lista


%---------%---------%---------%---------%---------%---------%---------%
% MINHEAP IN PROLOG %

Un MINHEAP è una struttura dati che prevede le sequenti operazioni: 
NEWHEAP, INSERT, HEAD, EXTRACT, MODIFYKEY. 


:- new_heap(H).
Questo predicato inserisce un nuovo heap nella base-dati Prolog.
Notate che il predicato heap(H, S) mantiene la dimensione corrente dello heap
nel secondo argomento.


:- delete_heap(H).
Rimuove tutto lo heap (incluse tutte le “entries”) dalla base-dati Prolog.


:- heap_has_size(H, S).
Questo predicato è vero quanto S è la dimensione corrente dello heap. 
Nell'implementazione di questo predicato abbiamo dovuto aggiungere il predicato
abolish/1 in quanto heaps_size/2 esisteva già nella libreria heaps di Prolog, 
creando notevoli problemi. 


:- heap_empty(H).
Questo predicato è vero quando lo heap H non contiene elementi.


:- heap_not_empty(H).
Questo predicato è vero quando lo heap H contiene almeno un elemento.


:- heap_head(H, K, V).
Il predicato head/3 è vero quando l’elemento dello heap H con chiave minima K è V


:- heap_insert(H, K, V).
Il predicato heap_insert/3 è vero quando l’elemento V è inserito nello heap H con chiave K.

:- heap_extract(H, K, V).
Il predicato heap_extract/3 è vero quando la coppia K, V con K minima, è rimossa dallo heap H.


:- modify_key(H, NewKey, OldKey, V).
Il predicato modify_key/4 è vero quando la chiave OldKey (associata al valore V) è sostituita da
NewKey. 


:- list_heap(H).
Il predicato richiama listing/1 per stampare sulla console Prolog lo stato
interno dello heap.

%% Predicati extra aggiunti da noi %%

:- heapify(H, Ip).
Effettua l'ordinamento di un nodo: controlla se esso deve essere
scambiato con un figlio (che ha chiave minore), in tal caso si richiama ricorsivamente
sull'indice del nodo scambiato.
Parte dal presupposto che il sotto-albero destro e sinistro sono gia degli heap corretti.

:- build_heap(_, L)
Questo predicato richiama l'heapify so ogni nodo fino alla radice

:-heap_insert_non_ordinata(H, K, V)
Questo predicato è uguale alla heap_insert, tuttavia dopo aver inserito l'elemento,
non effeuttua l'ordinamento

:-parent_swap(H,If).
Questo predicato  scambia il parent con il figlio, se quest'ultimo ha una chiave minore,
continua al massimo fino a raggiungere la radice.


%---------%---------%---------%---------%---------%---------%---------%
% MST IN PROLOG %

:-vertex_key(G, V, K).
Questo predicato è vero quando V è un vertice di G e, durante e dopo l’esecuzione dell’algoritmo di
Prim, contiene il peso minimo di un arco che connette V nell’albero minimo; se questo arco non
esiste (ed all’inizio dell’esecuzione) allora K è inf.
Questo predicato va dichiarato dynamic.


:-vertex_previous(G, V, U).
Questo predicato è vero quando V ed U sono vertici di G e, durante e dopo l’esecuzione
dell’algoritmo di Prim, il vertice U è il vertice “genitore” (“precedente”, o “parent”) di V nel
minimum spanning tree.
Questo predicato va dichiarato dynamic.

:-mst_prim(G, Source).
Questo predicato ha successo con un effetto collaterale. Dopo la sua prova, la base-dati Prolog ha
al suo interno i predicati vertex_key(G, V, k) per ogni V appartenente a G; la base-dati Prolog
contiene anche i predicati vertex_previous(G, V, U) per ogni V, ottenuti durante le iterazioni
dell’algoritmo di Prim. Naturalmente i predicati vertex_key(G, V, K) e
vertex_previous(G, V, U) devono essere corretti rispetto alla soluzione del problema MST.


:-mst_get(G, Source, PreorderTree).
Questo predicato è vero quando PreorderTree è una lista degli archi del MST ordinata secondo
un attraversamento preorder dello stesso, fatta rispetto al peso dell’arco. Attenzione che l’albero
non è binario e dovete ordinare archi con pari peso secondo l’ordinamento “lessicografico” del
vertice “target”.


%% Predicati extra aggiunti da noi %%

:-insert_arcs_in_heap([], _).
Predicato che inserisce una lista (Arcs) di archi nell'heap H

:-mst_heap_insert(H, New_arc).
Questo predicato, controlla se New_arc è già presente nel heap, in tal 
caso se P < Old_p effettua il modify key, in caso contrario non
modifica niente. 
Se New_arc non è presente lo inserisce.

:-heap_update(G, Vertex, H).
Predicato che aggiorna l'heap, in modo che contenga il peso minore per raggiungere ciascun nodo 
partendo da un nodo dell'MST.
Nota: aggiorna solo i collegamenti relativi al nuovo vertice del'mst,
perchè i precedenti sono rimasti uguali

:-mst_insert(G, H, Mst_Vs).
Questo predicato ricorsivo aggiunge ogni vertice del grafo all'mst (aggiungendo vertex-key e previous);
fino ad esaurire l'heap (e quindi i vertici che non sono nell'mst), 
oppure fin quando ci sono vertici raggiungibili.


:-mst_inizialize(G, H).
predicato che inserisce il primo elemento nell'mst, e inizializza il resto dell'heap

:-mst_heap_inizialize(G, H, [Vertex | Vertexs]).
predicato che inizializza l'heap con tutti i vertici e i relativi pesi messi all'infinito

:-mst_figli(G, Fs, Ln).
Questo predicato ricorsivo, presa una serie di vertici-figli, 
aggiungendo a Ln il preorder che enumera tali vertici. 
Si interrompe quando finisce la lista di vertici.


:-mst_get_arc(G, N, Arc)
% predicato che preso un nodo N, restituisce l'arco relativo


:-mst_output(G, P, Arc)
Questo predicato prende un vertice, e se esso non ha figli lo enumera, altrimenti
enumera pirma lui e poi i figli in ordine lessicografico.
Aggiunge ad Arc il preorder aggiornato.

















%---------%---------%---------%---------%---------%---------%---------%

L'intero elaborato è stato svolto affinchè il testo non superasse le 80
colonne secondo le direttive del prof (e così come il progetto, 
anche questo file rispetta tali norme).


%---------%---------%---------%---------%---------%---------%---------%


end of file -*- README.txt