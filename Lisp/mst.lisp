; Ismaele Nachit Le Voci, Luca Poli, Riccardo Moschi
(defparameter *heaps* (make-hash-table :test #'equal))
(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *heap-indexs* (make-hash-table :test #'equal))
(defparameter *vertex-keys* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))

(defun is-graph (graph-id)
	(gethash graph-id *graphs*))


(defun new-graph (graph-id)
	(or (gethash graph-id *graphs*)
		(setf (gethash graph-id *graphs*) graph-id))
		)


(defun delete-graph (graph-id)
	(if (is-graph graph-id)
		(progn
			(remhash graph-id *graphs*)
			(delete-hash-table *arcs* graph-id)
			(delete-hash-table *vertices* graph-id)
			(delete-hash-table *vertex-keys* graph-id)
			(delete-hash-table *previous* graph-id)
		)
	)
)

(defun delete-hash-table (hash-table graph-id)
	(if (or (equal hash-table *graphs*)
			(equal hash-table *arcs*)
			(equal hash-table *vertices*)
			(equal hash-table *vertex-keys*)
			(equal hash-table *previous*)
		)
		(progn (maphash #' (lambda (k v)
			(declare (ignore v))
			(if (equal (first k) graph-id)
				(remhash k hash-table)
			)
		) hash-table) t)
	nil)
)


(defun new-vertex (graph-id vertex-id)
	(if (is-graph graph-id)
		(if (not (gethash (list graph-id vertex-id) *vertices*))
			(setf (gethash (list graph-id vertex-id) *vertices*)
			(list graph-id vertex-id))
		t)
		(setf (gethash (list (new-graph graph-id) vertex-id) *vertices*)
		(list graph-id vertex-id))
	)
	(list 'vertex graph-id vertex-id)
)


(defun graph-vertices (graph-id)
	(let ((l nil)) (progn (maphash #'(lambda (k v)
		(if (equal (first k) graph-id)
		(push (append (list 'vertex) v) l)
		)
	) *vertices*) l)))

;arc
(defun new-arc (graph-id u v weight)
	(if (not (equal u v))
		(progn
			(new-vertex graph-id u)
			(new-vertex graph-id v)
			(if (and (not (gethash (list graph-id u v) *arcs*))
					 (not (gethash (list graph-id v u) *arcs*))
				)
				(setf (gethash (list graph-id u v) *arcs*) weight)
				(if (gethash (list graph-id u v) *arcs*)
					(setf (gethash (list graph-id u v) *arcs*) weight)
					(setf (gethash (list graph-id v u) *arcs*) weight)
				)
			)
		)
	)
	(list 'arc graph-id u v weight)
)


(defun graph-arcs (graph-id)
 	(let ((l nil)) (progn (maphash #'(lambda (k v)
		(if (equal (first k) graph-id)
			(push (append (list 'arc) k (list v))
				l)
		)
	) *arcs*) l)
))


(defun graph-vertex-neighbors (graph-id vertex-id)
 	(let ((l nil)) (progn (maphash #'(lambda (k v)
		(if (and (equal (first k) graph-id)
 			 (equal (second k) vertex-id))
			(push (append (list 'arc) k (list v))
				l)

			(if (and (equal (first k) graph-id)
				(equal (third k) vertex-id))
				(push (append (list 'arc) k (list v))
					l)

		))
	) *arcs*)
l)))

(defun graph-vertex-adjacent (graph-id vertex-id)
	(let ((l nil)) (progn (maphash #'(lambda (k v) (declare (ignore v))
		(if (and (equal (first k) graph-id)
				(equal (second k) vertex-id))
			(push (append (list 'vertex) (list graph-id)(list (third k)))
					l)
			(if (and (equal (first k) graph-id)
				(equal (third k) vertex-id))
				(push (append (list 'vertex) (list graph-id)(list (second k)))
						l)
		))
	) *arcs*)
l)))

(defun read-graph(graph-id source)
	(new-graph graph-id)
	(load source)
)

(defun graph-find-parent (graph-id vertex-id weight)
	(let ((parent nil)) (progn
		(maphash #'(lambda (k v)
			(if (and (equal (first k) graph-id)
					(equal (second k) vertex-id)
					(equal v weight)
				)
				(setf parent (third k))

			(if (and (equal (first k) graph-id)
					(equal (third k) vertex-id)
					(equal v weight)
				)
				(setf parent (second k))
			)
			)
		)*arcs*)
	parent))
)

(defun vertex-adjacent-no-mst (graph-id vertex-id)
	(let ((l nil)) (progn
		(maphash #'(lambda (k v)
			(if (and (equal (first k) graph-id)
					(equal (second k) vertex-id)
					(not(gethash (list graph-id (third k)) *vertex-keys*)))
				(push (append (list 'arc) k (list v))
					l)

			(if (and (equal (first k) graph-id)
					(equal (third k) vertex-id)
					(not (gethash (list graph-id (second k)) *vertex-keys*)))
				(push (append (list 'arc) k (list v))
					l)
			)
			)
		)*arcs*)
	l))
)


(defun graph-print(graph-id)
	(maphash #'(lambda (k v) (if (string= (car k) graph-id) (print (list k v)))) *arcs*)
)





;minheap
(defun heap-id (heap-rep)
	(second heap-rep))


(defun heap-size (heap-rep)
	(third heap-rep))


(defun heap-actual-heap (heap-rep)
	(fourth heap-rep))


(defun new-heap (heap-id capacity)
	(or (gethash heap-id *heaps*)
	(setf (gethash heap-id *heaps*)
	(list 'heap heap-id capacity (make-array capacity :adjustable t)))))


(defun delete-heap (heap-id)
	(if (gethash heap-id *heaps*)
		(progn (remhash heap-id *heaps*)
			(maphash #' (lambda (k v)
				(declare (ignore v))
				(if (equal (first k) heap-id)
					(remhash k *heap-indexs*)
				)
			) *heap-indexs*)
		t)
	)
)


(defun heap-print (heap-id)
	(if (gethash heap-id *heaps*)
		(print(heap-actual-heap (gethash heap-id *heaps*)))
	)
)


(defun heap-empty(heap-id)
	(if (zerop (heap-size (gethash heap-id *heaps*))) T nil))


(defun heap-not-empty(heap-id)
	(if (zerop (heap-size (gethash heap-id *heaps*))) nil T))


(defun heap-head (heap-id)
  (and (gethash heap-id *heaps*)
  (aref (heap-actual-heap (gethash heap-id *heaps*)) 0)))


(defun heap-modify-size (heap-id new-size)
	(if (gethash heap-id *heaps*)
			(setf (gethash heap-id *heaps*)
				(list 'heap
					(heap-id
						(gethash heap-id *heaps*))
					new-size
					(adjust-array
						(heap-actual-heap
							(gethash heap-id *heaps*))
						new-size)))
))

(defun heap-increase-size (heap-id)
	(if (gethash heap-id *heaps*)
		(heap-modify-size heap-id
			(+ (heap-size (gethash heap-id *heaps*)) 1))))


(defun heap-decrease-size (heap-id)
	(if (gethash heap-id *heaps*)
		(heap-modify-size heap-id
			(- (heap-size (gethash heap-id *heaps*)) 1))))


(defun heap-left-index (parent)
	(+ (* parent 2) 1)
)


(defun heap-right-index (parent)
	(+ (* parent 2) 2)
)


(defun heap-get-value (heap-id index)
	(aref (heap-actual-heap(gethash heap-id *heaps*)) index)
)


(defun heap-get-left (heap-id parent)
	(if (>= (heap-left-index parent) (heap-size (gethash heap-id *heaps*)))
		(aref (heap-actual-heap (gethash heap-id *heaps*)) parent)
		(aref (heap-actual-heap (gethash heap-id *heaps*))
			(heap-left-index parent))
	)
)


(defun heap-get-right (heap-id parent)
	(if (>= (heap-right-index parent) (heap-size (gethash heap-id *heaps*)))
		(aref (heap-actual-heap (gethash heap-id *heaps*)) parent)
		(aref (heap-actual-heap (gethash heap-id *heaps*))
			(heap-right-index parent))
	)
)


(defun heap-set-value (heap-id index newVal)
	(setf (aref (heap-actual-heap(gethash heap-id *heaps*)) index) newVal)
)


(defun heap-set-left (heap-id parent newVal)
	(if (< (heap-left-index parent) (heap-size (gethash heap-id *heaps*)))
		(setf (aref (heap-actual-heap(gethash heap-id *heaps*))
			(heap-left-index parent)) newVal)
	)
)


(defun heap-set-right (heap-id parent newVal)
	(if (< (heap-right-index parent) (heap-size (gethash heap-id *heaps*)))
		(setf (aref (heap-actual-heap(gethash heap-id *heaps*))
			(heap-right-index parent)) newVal)
	)
)

(defun heap-update-index (heap-id value newIndex)
	(setf (gethash (list heap-id value) *heap-indexs*) newIndex)
)

(defun heap-get-index (heap-id value)
	(gethash (list heap-id value) *heap-indexs*)
)

(defun heap-delete-index (heap-id value)
	(remhash (list heap-id value) *heap-indexs*)
)

(defun heap-swap-position (heap-id index1 index2)
	(and
	(heap-set-value heap-id index1
		(prog1
			(heap-get-value heap-id
				(heap-update-index heap-id
					(second (heap-get-value heap-id index1)) index2))
			(heap-set-value heap-id index2
			(heap-get-value heap-id
				(heap-update-index heap-id
					(second (heap-get-value heap-id index2)) index1))
			)
		)
	)
	(list index1 index2))
)


(defun heap-extract (heap-id)
	(if (and (gethash heap-id *heaps*)
			(> (heap-size (gethash heap-id *heaps*)) 0))
		(if (= (heap-size (gethash heap-id *heaps*)) 1)
			(prog2
				(heap-delete-index heap-id (second(heap-head heap-id)))
				(heap-head heap-id)
				(heap-decrease-size heap-id))
			(prog2
				(heap-delete-index heap-id (second(heap-head heap-id)))
				(heap-head heap-id)
				(heap-set-value heap-id
					(heap-update-index heap-id
						(second (heap-get-value heap-id
							(- (heap-size (gethash heap-id *heaps*)) 1))
						)
					0)
					(heap-get-value heap-id
						(- (heap-size (gethash heap-id *heaps*)) 1)
					)
				)
				(heapify (heap-id (heap-decrease-size heap-id) ) 0)
			)
		)
	)
)


(defun heapify (heap-id index)
	(if (gethash heap-id *heaps*)
		(if (and (< (first (heap-get-left heap-id index))
					(first (heap-get-value heap-id index)))
			(< (first (heap-get-left heap-id index))
				(first (heap-get-right heap-id index))))
			(heapify heap-id
				(second (heap-swap-position heap-id index
					(heap-left-index index))))
			(if (< (first (heap-get-right heap-id index))
					(first (heap-get-value heap-id index)))
				(heapify heap-id
					(second (heap-swap-position heap-id index
						(heap-right-index index)))
				)
			t)
		)
	)
)


(defun build-heap (heap-id index)
	(if (>= index 0)
		(if (heapify heap-id index)
			(build-heap heap-id (- index 1))
		)
	t)
)


(defun heap-insert (heap-id key value)
	(if (gethash heap-id *heaps*)
		(and
			(heap-update-index
				heap-id value (heap-size (gethash heap-id *heaps*)))
			(heap-set-value (heap-id (heap-increase-size heap-id))
				(- (heap-size (gethash heap-id *heaps*)) 1)
				(list key value) )
			(build-heap heap-id
				(- (floor (heap-size (gethash heap-id *heaps*)) 2) 1) )
		)
	)
)


(defun heap-insert-non-ordinata (heap-id key value)
	(if (gethash heap-id *heaps*)
		(and
			(heap-update-index
				heap-id value (heap-size (gethash heap-id *heaps*)))
			(heap-set-value (heap-id (heap-increase-size heap-id))
			(- (heap-size (gethash heap-id *heaps*)) 1)
			(list key value) ) t)
	)
)


(defun parent-swap (heap-id figlio)
	(if (< figlio (heap-size (gethash heap-id *heaps*)))
		(if (= figlio 0) 0
			(if (< (first (heap-get-value heap-id figlio))
				   (first (heap-get-value heap-id (floor (- figlio 1) 2))))
				(parent-swap heap-id
					(second( heap-swap-position heap-id figlio
							(floor (- figlio 1) 2)))
				)
			figlio)
		)
	)
)


(defun find-ric (heap-id element &optional (index 0))
	(if (< index (heap-size (gethash heap-id *heaps*)) )
		(if (equal element (heap-get-value heap-id index)) index
			(if (>= (first element) (first (heap-get-value heap-id index)))
				(or (find-ric heap-id element (heap-left-index index))
					(find-ric heap-id element (heap-right-index index)))
			)
		)
	)
)

(defun heap-find-element (heap-id value)
	(if (gethash heap-id *heaps*)
		(heap-get-index heap-id value)
	)
)

(defun heap-modify-key (heap-id newKey oldKey value)
	(if(gethash heap-id *heaps*)
		(if oldKey
			(let ((index (find-ric heap-id (list oldKey value))))
				(progn (heap-set-value heap-id index (list newKey value))
					(heapify heap-id (parent-swap heap-id index))
				)
			)
			(progn
				(heap-set-value heap-id
					(heap-find-element heap-id value)
					(list newKey value))
				(heapify heap-id (parent-swap heap-id
					(heap-find-element heap-id value)))
			)
		)
	)
)

(defun heap-random-name (&optional (n 1))
	(let ((value (random n)))
		(if (gethash value *heaps*)
		(heap-random-name (* n 10))
		value
		)
	)
)


;MST

(defun new-vertex-key (graph-id vertex weight)
	(setf (gethash (list graph-id vertex) *vertex-keys*) weight)
)


(defun vertex-key (graph-id vertex)
	(gethash (list graph-id vertex) *vertex-keys*)
)


(defun vertex-previous (graph-id vertex)
	(gethash (list graph-id vertex) *previous*)
)


(defun new-vertex-previous (graph-id vertex parent)
	(setf (gethash (list graph-id vertex) *previous*) parent)
)

(defun mst-inizialize (graph-id heap-id)
  (let ((l (graph-vertices graph-id)))
  	(prog1
		(third (first l))
		(new-vertex-key graph-id (third (first l)) 0)
		(new-vertex-previous graph-id (third (first l)) nil)
		(mst-heap-inizialize graph-id heap-id (cdr l))
	)
  )
)


(defun mst-heap-inizialize(graph-id heap-id vertices)
	(if (> (length vertices) 0)
		(and (heap-insert-non-ordinata heap-id MOST-POSITIVE-DOUBLE-FLOAT
				(third (first vertices)))
			(mst-heap-inizialize graph-id heap-id (cdr vertices)))
	t)
)

(defun mst-heap-insert (heap-id newArc vertex-id)
	(let ((oldWeight (first (heap-get-value heap-id
			(heap-find-element heap-id
						 	(if (equal (third newArc) vertex-id)
						 			(fourth newArc) (third newArc)
						 	)
			)
		))))
		(if (< (fifth newArc) oldWeight)
			(heap-modify-key heap-id (fifth newArc) nil
				(if (equal (third newArc) vertex-id)
					   (fourth newArc) (third newArc)
				)
			)
		t)
	)
)


(defun insert-arcs-in-heap (heap-id arcs vertex-id)
	(if (> (length arcs) 0)
		(and (mst-heap-insert heap-id (car arcs) vertex-id)
			(insert-arcs-in-heap heap-id (cdr arcs) vertex-id)
		)
	t)
)


(defun heap-update (graph-id vertex-id heap-id)
	(insert-arcs-in-heap heap-id
		(vertex-adjacent-no-mst graph-id vertex-id) vertex-id)
)


(defun mst-insert (graph-id heap-id last-vertex)
	(if (and (heap-not-empty heap-id)
			(< (first (and (heap-update graph-id last-vertex heap-id)
						(heap-head heap-id)))
			MOST-POSITIVE-DOUBLE-FLOAT)
		)
		(and (new-vertex-key graph-id (second (heap-head heap-id))
				(first (heap-head heap-id)))
			(new-vertex-previous graph-id (second (heap-head heap-id))
			 	(graph-find-parent graph-id (second (heap-head heap-id))
				 	(first (heap-head heap-id))
                )
			)
			(mst-insert graph-id heap-id (second (heap-extract heap-id)))
		)
	t)
)


(defun mst-prim (graph-id source)
	(if (read-graph graph-id source)
		(let ((heap-id (heap-random-name)))
			(mst-insert graph-id heap-id
				(mst-inizialize graph-id (second (new-heap heap-id 0)))
			)
		)
	)
)


(defun mst-prim-no-load (graph-id)
		(let ((heap-id (heap-random-name)))
			(mst-insert graph-id heap-id
				(list (mst-inizialize graph-id (second (new-heap heap-id 0))))
			)
		)
)


(defun mst-get-radice (graph-id)
	(let (( l nil))
		(progn (maphash #'(lambda (k v)
			(if (and(equal (first k) graph-id) (not v) )
				(setf l (second k ))))
		*previous*)
	l))
)


(defun mst-get-arc (graph-id vertex)
	(list 'arc graph-id (vertex-previous graph-id vertex)
		vertex (vertex-key graph-id vertex))
)


(defun get-figli (graph-id vertex)
	(let (( l nil))
		(progn(maphash #'(lambda (k v)
				(if (and(equal (first k) graph-id) (equal vertex v ))
					(setf l (append l (list(second k))))))
		*previous*)
	l))
)


(defun mst-figli (graph-id vertexs)
	(if (> (length vertexs) 0)
		(append (mst-output graph-id (first vertexs))
			(mst-figli graph-id (cdr vertexs)))
	)
)


(defun mst-output (graph-id parent)
	(let ((figli (get-figli graph-id parent)))
		(if (not figli)
			(list (mst-get-arc graph-id parent))
			(append (list (mst-get-arc graph-id parent))
				(mst-figli graph-id (sort figli 'string<)))
		)
	)
)


(defun mst-get (graph-id source)
	(declare (ignore source))
	(append (list (mst-get-arc graph-id (mst-get-radice graph-id)))
		(mst-figli graph-id (sort (get-figli graph-id (mst-get-radice graph-id)) 'string<))
	)
)
