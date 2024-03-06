% -*- Mode: Lisp-*-
% README.txt


%---------%---------%---------%---------%---------%---------%---------%
% INTRODUZIONE %

Nome progetto: mst.lisp

Obbiettivo: Lo scopo di questo progetto è di implementare l’algoritmo di Prim  per la soluzione
del problema MST per grafi non-diretti e connessi con pesi non negativi. Per procedere all’implementazione 
di questi algoritmi è necessario produrre un’implementazione di un MINHEAP.


Conoscenze necessarie per la comprensione del progetto:
Conoscenze base di programmazione in linguaggio Lisp
Conoscenze grafi, heap, heapsort.


%---------%---------%---------%---------%---------%---------%---------%

Legenda:
La simbologia "%% Predicati extra aggiunti da noi %%" prima di un predicato indica che quel 
predicato non era stato espressamente richiesto dal professore ma necessario 
per il funzionamento del tutto.


%---------%---------%---------%---------%---------%---------%---------%


Progetto scolastico livello universitario a cura di:

Luca Poli 
Ismaele Nachit Le Voci
Riccardo Moschi


%---------%---------%---------%---------%---------%---------%---------%


Linguaggio: Lisp


%---------%---------%---------%---------%---------%---------%---------%
Metodo di utilizzo:

Si può usare un grafo letto da un file o inserito a mano.

% GRAFO INSERITO MANUALMENTE % 

Si possono usare le funzioni adibite alla creazione, come new-graph,
new-vertex e new-arc.
Dopodichè è sufficente invocare la funzione mst-prim-no-load passando il grafo relativo.

(mst-prim-no-load graph-id)

% GRAFO CARICATO DA FILE %

Il file deve essere un .lisp che contiene le chiamate alle funzioni adibite
come new-graph, new-vertex e new-arc.
Dopodichè è sufficente invocare la funzione mst-prim passando il grafo creato nel file.

(mst-prim graph-id source)

Ad esempio:

(new-arc 'g 'a 'b 1)
(new-arc 'g 'a 'd 7)

%---------%---------%---------%---------%---------%---------%---------%

Considerazioni riguardo alla hash-table aggiunta: 

heap-indexs: 
Questa hash-table contiene come chiave (heap-id valore-vertice) e come valore (index)
serve per associare ad ogni vertice nell'heap il suo indice.
In questo modo si vanno a velocizzare le operazioni di ricerca


%---------%---------%---------%---------%---------%---------%---------%
Spiegazione dei predicati utilizzati:


% INTERFACCIA LISP PER LA MANIPOLAZIONE DEI GRAFI %


(defun  is-graph(graph-id)).
Questa funzione ritorna il graph-id stesso se questo grafo è già stato creato, oppure NIL se
no. 

(defun new-graph(graph-id)).
Questa funzione genera un nuovo grafo e lo inserisce nel data base (ovvero nella hash-table)
dei grafi. Restituisce il graph-id


(defun  delete-graph(graph-id)).
Rimuove l’intero grafo dal sistema (vertici archi etc); ovvero rimuove tutte le istanze
presenti nei data base (ovvero nelle hash-tables) del sistema.
Restuisce T se il grafo è stato trovato, altrimenti nil.


(defun  new-vertex (graph-id vertex-id)).
Aggiunge un nuovo vertice vertex-id al grafo graph-id. Notate come la
rappresentazione di un vertice associ un vertice ad un grafo (o più).

(defun  graph-vertices (graph-id)).
Questa funzione torna una lista di vertici del grafo.


(defun new-arc(graph-id u v & optional weight)).
Questa funzione aggiunge un arco del grafo graph-id nella hash-table *arcs*.
Non effettua l'inserimento se l'arco (o il suo speculare) esistono già.
Nel caso i vertici o il grafo non siano stati creati provvederà anche a quello.
Resituisce una arc-rep

(defun graph-arcs(graph-id)).
Questa funzione ritorna una lista una lista di tutti gli archi presenti in graph-id.

(defun graph-vertex-neighbors(graph-id vertex-id)).
Questa funzione ritorna una lista arc-rep-list contenente gli archi
che portano ai vertici immediatamente raggiungibili da vertex-id.

(defun graph-vertex-adjacent(graph-id vertex-id)).
Questa funzione ritorna una lista vertex-rep-list contenente i vertici
adiacenti a vertex-id.

(defun graph-print graph-id)
Questa funzione stampa alla console dell’interprete Common Lisp una lista dei vertici e degli
archi del grafo graph-id

%% Predicati extra aggiunti da noi %%


(defun delete-hash-table (hash-table graph-id)).
Questa funzione cancella da una delle hash table graph, arcs, vertex, 
vertex-keys o vertex-previous tutti i dati relativi al grafo passato.
Restituisce t se la hash table passata è valida, altrimenti nil

(defun read-graph(graph-id source))
Questa funzione legge il grafo passato come source
Restituisce T.

(defun graph-find-parent (graph-id vertex-id weight)).
Questa funzione ricerca dato il grafo, un vertice e il peso,
quel'e il nodo che si collega al vertice con quel peso;
e lo restituisce.

(defun vertex-adjacent-no-mst (graph-id vertex-id))
Questa funzione è come la vertex-adjacent, ma esclude
dalla lista gli archi che portano a vertici dell'mst.

%---------%---------%---------%---------%---------%---------%---------%
% MINHEAP IN LISP%

Un MINHEAP è una struttura dati che prevede le sequenti operazioni: 
NEWHEAP, INSERT, HEAD, EXTRACT, MODIFYKEY. 


(defun heap-id(heap-rep)).
Restituisce l'heap id.

(defun new-heap(heap-id)).
Questa funzione inserisce un nuovo heap nella hash-table *heaps*.

(defun heap-size(heap-rep))
Restituisce l'heap size.

(defun heap-actual-heap(heap-rep))
Restituisce l'heap vero e proprio

(defun delete_heap(heap-id)).
Rimuove tutto lo heap indicizzato da heap-id.
Restituisce T se l'heap è stato trovato, altrimenti nil.


(defun heap-print(heap-id)).
Effettua la stampa dell'heap  --ad-a-d-ad-a


(defun heap-empty(heap-id)).
Questa funzione quando lo heap heap-id non contiene elementi restituisce T, altrimenti nil.


(defun heap-not-empty(heap-id)).
Questa funzione quando lo heap heap-id contiene almeno un elemento restituisce T, altrimenti nil.

(defun heap-head(heap-id)).
La funzione heap-head ritorna una lista di due elementi dove K è la chiave minima e V il valore
associato.

(defun heap-extract(heap-id))
La funzione heap-extract ritorna la lista con K, V e con K minima; la coppia è rimossa dallo
heap heap-id.

(defun heap-insert (heap-id key value))
La funzione heap-insert inserisce l’elemento value nello heap heap-id con chiave key.
Restituisce T se l'heap è stato trovato, altrimenti nil.

(defun modify-key(heap-id newKey oldKey value))
La funzone heap-modify-key sostituisce la chiave oldKey (associata al valore value) con
NewKey. Se oldKey verra passata come nil, non sarà usata nella ricerca del valore.
Restituisce T.

(defun heap-print(heap-id ))
Questa funzione stampa sulla console lo stato interno dello heap heap-id. 


%% Predicati extra aggiunti da noi %%

(defun heapify(heap-id index))
Restituisce T. Effettua l'ordinamento di un nodo: controlla se esso deve essere
scambiato con un figlio (che ha chiave minore), in tal caso si richiama ricorsivamente
sull'indice del nodo scambiato.
Parte dal presupposto che il sotto-albero destro e sinistro sono gia degli heap corretti.

(defun build-heap (heap-id index))
Restituisce T. Effettua l'ordinamento di tutto l'heap, richiamando heapify dal primo nodo
non foglia fino alla radice.

(defun heap-modify-size(heap-id new-size)).
Modifica la dimensione dell'heap, portandola a quella passata come new-size. aggiornando
anche la hash-table relativa.
è possibile ridimensionare l'heap, perchè esso è definito come adjustable.
Restituisce la heap-rep se l'heap è stato trovato, altrimenti nil.

(defun heap-increase-size(heap-id))
Questa funzione richiama la heap-modify-size per aumentare la dimensione dell'heap di 1.
Restituisce la heap-rep

(defun heap-decrease-size(heap-id))
Questa funzione richiama la heap-modify-size per diminuire la dimensione dell'heap di 1.
Restituisce la heap-rep

(defun heap-left-index(parent)).
Questa funzione prende l'indice del parent e restuisce l'indice che porta al figlio sinistro.

(defun heap-right-index(parent)).
Questa funzione prende l'indice del parent e restuisce l'indice che porta al figlio destro.

(defun heap-get-value(heap-id index)).
Questa funzione restituisce la coppia chiave-valore dell'indice passato.

(defun heap-get-left(heap-id parent))
Questa funzione restituisce la coppia chiave-valore del figlio sinistro
ripsetto all'indice passato.
Nota: se il figlio non è presente si restituisce il valore del parent

(defun heap-get-right(heap-id parent))
Questa funzione restituisce la coppia chiave-valore del figlio destro
ripsetto all'indice passato.
Nota: se il figlio non è presente si restituisce il valore del parent

(defun heap-set-value(heap-id index newVal))
Questa funzione modifica la coppia chiave-valore dell'indice passato, 
inserendo al suo posto newVal.
Restituisce newVal.

(defun heap-set-left(heap-id index newVal))
Questa funzione modifica la coppia chiave-valore del figlio sinistro 
dell'indice passato, inserendo al suo posto newVal.
Restituisce newVal.
Nota: se il figlio sinistro non è presente, non verrano effetuate modifiche.

(defun heap-set-right(heap-id index newVal))
Questa funzione modifica la coppia chiave-valore del figlio destro
dell'indice passato, inserendo al suo posto newVal.
Restituisce newVal.
Nota: se il figlio destro non è presente, non verrano effetuate modifiche.

(defun heap-update-index (heap-id value newIndex))
Questa funzione modifica l'indice associato ad un valore dell'heap 
(nella hash-table relativa) aggiornandolo con il nuovo indice.
Restituisce il nuovo indice.

(defun heap-get-index (heap-id value))
Questa funzione restituisce l'indice associato ad un valore dell'heap 
(nella hash-table relativa).

(defun heap-delete-index (heap-id value))
Questa funzione cancella l'indice associato ad un valore dell'heap 
(nella hash-table relativa), eliminando anche la rispettiva chiave.
Restituisce T.

(defun heap-insert-non-ordinata (heap-id key value))
Questa funzione effettua l'inserimento di una coppia chiave-valore nell'heap;
ma non la ordina, perdendo quindi la heap-property.
Restituisce T se l'heap è stato trovato, altrimenti nil.

(defun parent-swap (heap-id figlio))
Questa funzione prende l'indice di un nodo figlio e se la sua chiave è minore rispetto
a quella del padre, li scambia; in tal caso si richiama ricorsivamente sull'indice del parent
Restituisce l'indice del figlio, altrimenti se il figlio non era valido restituisce nil.

(defun find-ric (heap-id element &optional (index 0))
Questa funzione effettua la ricerca (in modo ricorsivo e lineare) dell'elemento, restituendo
la sua posizione.

(defun heap-find-element(heap-id value))
Questa funzione sfrutta la hash-table *heap-indexs* per ottenere l'indice di un determinato valore
e lo restituisce.

(defun heap-swap-position(heap-id index1 index2))
Questa funzione scambia di posizione gli elementi contenuti agli indici passati;
aggiorna anche la hash-table *heap-indexs*.
Restituisce la coppia index1-index2.

(defun heap-random-name (&optional (n 1))
Questa funzione genera un identificatore randomico per un heap, e lo restituisce.
Naturalmente non genera identificatori a heap già usati.


%---------%---------%---------%---------%---------%---------%---------%
% MST IN LISP%


(defun vertex-key (graph-id vertex))
Questa funzione, dato un vertex-id di un grafo graph-id ritorna, durante e dopo l’esecuzione
dell’algoritmo di Prim, il peso minimo di un arco che connette vertex-id nell’albero minimo; se
questo arco non esiste (ed all’inizio dell’esecuzione) allora k è MOST-POSITIVE-DOUBLE-FLOAT

(defun vertex-previous (graph-id vertex))
Questa funzione, durante e dopo l’esecuzione dell’algoritmo di Prim, ritorna il vertice U che il
vertice “genitore” (“precedente”, o “parent”) di V nel minimum spanning tree V


(defun mst-prim  (graph-id source))
Questa funzione termina con un effetto collaterale. Dopo la sua esecuzione, la hash-table
*vertex-key* contiene al suo interno le associazioni (graph-id V) ⇒ d per ogni V
appartenente a graph-id; la hash-table *previous* contiene le associazioni
(graph-id V) ⇒ U calcolate durante l’esecuzione dell’algoritmo di Prim.


(defun mst-get (graph-id))
Questa funzione ritorna preorder-mst che è una lista degli archi del MST ordinata secondo un
attraversamento preorder dello stesso, fatta rispetto al peso dell’arco.



%% Predicati extra aggiunti da noi %%

(defun new-vertex-key (graph-id vertex weight))
Questa funzione aggiunge un vertice col peso alla hashtable *vertex-keys*, e lo restituisce

(defun vertex-key (graph-id vertex))
Questa funzione prende un vertice e restituisce il peso con cui si collega all'mst

(defun new-vertex-previous (graph-id vertex parent))
Questa funzione un vertice e il relativo parent alla hashtable *previous*, e lo restituisce

(defun vertex-previous (graph-id vertex))
Questa funzione prende un vertice e restituisce il parent con cui si collega all'mst

(defun graph-find-parent (vertex weight arcs))

(defun mst-inizialize (graph-id heap-id))
Questa funzione inizializza l'mst inserendo il primo vertice del grafo nell'mst;
per poi inizializzare l'heap.
Restiuisce il valore del primo vertice inserito.

(defun mst-heap-inizialize(graph-id heap-id vertices))
Quasta funzione inizializza l'heap, inserendo ogni vertice del grafo nell'heap
con peso MOST-POSITIVE-DOUBLE-FLOAT.
Restituisce T o nil.

(defun heap-update (graph-id vertex-id heap-id))
Questa funzione modifica l'heap aggiornando i pesi dei vertici, 
usando l'ultimo vertice inserito nell'Mst (che viene passato come vertex-id).
Restituisce T.

(defun insert-arcs-in-heap (heap-id arcs vertex-id))
Questa funzione ricorsiva prende gli archi che portano ai vertici adiacenti che non sono nell'mst
e che partono dall'ultimo vertice aggiunto all'MST (e che viene passato).
Per ogni arco si andrà ad aggiornare il peso relativo al vertice nell'heap.

(defun mst-heap-insert (heap-id newArc vertex-id))
Questa funzione prende in input un arco e un vertice dell'mst; e va ad aggiornare il peso
del vertice (non quello dell'mst), relativo all'arco passato.
L'aggiornamento avviene solo se il peso di questo arco è minore rispetto a quello salvato nell'heap.
Restituisce T.

(defun mst-insert (graph-id heap-id last-vertex)
Questa funzione ricorsiva aggiunge ogni vertice del grafo all'mst (aggiungendo vertex-key e previous);
fino ad esaurire l'heap (e quindi i vertici che non sono nell'mst), 
oppure fin quando ci sono vertici raggiungibili.
Restituisce T.


(defun mst-prim-no-load (graph-id))
Questa funzione effettua la mst-prim senza caricare un grafo da un file


(defun mst-get-radice (graph-id))
Questa funzione restituisce il valore della radice (del rispettivo grafo) dell'mst.


(defun mst-get-arc (graph-id vertex))
Questa funzione prende un vertice e restituisce l'arco, dell'mst, che lo collega 
tramite il previous e il peso minimo la resto dell'mst


(defun get-figli (graph-id vertex))
Questa funzione restituisce una lista contenente i nodi figli di un vertice dell'mst.


(defun mst-figli (graph-id vertexs))
Questa funzione ricorsiva, presa una serie di vertici-figli, 
restituisce il preorder che enumera tali vertici. 
Si interrompe quando finisce la lista di vertici


(defun mst-output (graph-id parent))
Questa funzione prende un vertice, e se esso non ha figli lo enumera, altrimenti
enumera pirma lui e poi i figli in ordine lessicografico.
Restituisce il preorder aggiornato.





%---------%---------%---------%---------%---------%---------%---------%

L'intero elaborato è stato svolto affinchè il testo non superasse le 80
colonne secondo le direttive del prof (e così come il progetto, 
anche questo file rispetta tali norme).


%---------%---------%---------%---------%---------%---------%---------%




end of file -*- README.txt