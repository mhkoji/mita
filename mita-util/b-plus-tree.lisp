;; ref: https://www.geeksforgeeks.org/insertion-in-a-b-tree/
(defpackage :mita.util.b+tree
  (:use :cl)
  (:export :search
           :delete
           :insert
           :make-tree)
  (:shadow :search
           :delete
           :copy-tree))
(in-package :mita.util.b+tree)

(defstruct node id keys ptrs size leaf-p)

(defstruct tree
  root
  (node-id 1000)
  nodes)

(defvar *MAX* 3)

(defun new-node (tree leaf-p)
  (let ((node (make-node
               :id (incf (tree-node-id tree))
               :keys (make-array *MAX* :initial-element nil)
               :ptrs (make-array (1+ *MAX*) :initial-element nil)
               :leaf-p leaf-p)))
    (push node (tree-nodes tree))
    node))

(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (prin1 (list :id (node-id obj)
                 :keys (subseq (node-keys obj)
                               0 (node-size obj))
                 :ptrs (map 'vector
                            (lambda (n)
                              (when n (node-id n)))
                            (subseq (node-ptrs obj)
                                    0 (1+ (node-size obj))))
                 :leaf-p (node-leaf-p obj))
           stream)))


(defun search-leaf-node (tree x)
  (let ((parent nil)
        (node (tree-root tree)))
    (loop while (and node (not (node-leaf-p node))) do
      (with-accessors ((keys node-keys)
                       (ptrs node-ptrs)
                       (size node-size)) node
        (setq parent node)
        (loop for i from 0 below size
              if (< x (aref keys i))
                do (progn (setq node (aref ptrs i))
                          (return))
              if (= i (1- size))
                do (progn (setq node (aref ptrs (1+ i)))
                          (return)))))
    (values node parent)))

(defun find-parent (node child)
  (if (or (node-leaf-p node)
          (node-leaf-p (aref (node-ptrs node) 0)))
      nil
      (loop for i from 0 to (node-size node) do
        (progn
          (when (eq (aref (node-ptrs node) i) child)
            (return-from find-parent node))
          (let ((parent (find-parent (aref (node-ptrs node) i) child)))
            (when parent
              (return-from find-parent parent)))))))

(defun insert-to-parent (tree node x child)
  (if (< (node-size node) *MAX*)
      (with-accessors ((keys node-keys)
                       (ptrs node-ptrs)
                       (size node-size)) node
        (let ((i 0))
          (loop while (and (< i size) (< (aref keys i) x))
                do (incf i))
          (loop for j from (+ size 0) downto (+ i 1)
                do (setf (aref keys j) (aref keys (1- j))))
          (loop for j from (+ size 1) downto (+ i 2)
                do (setf (aref ptrs j) (aref ptrs (1- j))))
          (setf (aref keys i) x)
          (incf size)
          (setf (aref ptrs (1+ i)) child)))

      (let ((keys (node-keys node))
            (ptrs (node-ptrs node))
            (new-internal (new-node tree nil))
            (virtual-key (make-array (+ *MAX* 1) :initial-element nil))
            (virtual-ptr (make-array (+ *MAX* 2) :initial-element nil)))
        (loop for i from 0 below *MAX* do
          (setf (aref virtual-key i) (aref keys i)))
        (loop for i from 0 below (1+ *MAX*) do
          (setf (aref virtual-ptr i) (aref ptrs i)))
        (let ((i 0))
          (loop while (and (< i *MAX*)
                           (< (aref virtual-key i) x))
                do (incf i))
          (loop for j from (+ *MAX* 0) downto (+ i 1)
                do (setf (aref virtual-key j) (aref virtual-key (1- j))))
          (setf (aref virtual-key i) x)
          (loop for j from (+ *MAX* 1) downto (+ i 2)
                do (setf (aref virtual-ptr j) (aref virtual-ptr (1- j))))
          (setf (aref virtual-ptr (1+ i)) child)
          (setf (node-size node)
                (floor (1+ *MAX*) 2))
          (setf (node-size new-internal)
                (- ;; the number of the total keys
                   (1+ *MAX*)
                   ;; the left-most key is not needed
                   1
                   ;; the number of the keys on the other nodes))
                   (node-size node)))
          (loop for i from 0 below (node-size node)
                do (setf (aref (node-keys node) i)
                         (aref virtual-key i)))
          (loop for i from 0 below (1+ (node-size node))
                do (setf (aref (node-ptrs node) i)
                         (aref virtual-ptr i)))
          (loop for i from 0 below (node-size new-internal)
                ;; node-size + 1 because the left-most key is not needed
                for j from (+ (node-size node) 1) do
            (setf (aref (node-keys new-internal) i)
                  (aref virtual-key j)))
          (loop for i from 1 below (1+ (node-size new-internal))
                for j from (+ 1 (node-size node)) do
            (setf (aref (node-ptrs new-internal) i)
                  (aref virtual-ptr j)))
          ;; new-x is between the updated node and the new-internal node
          (let ((new-x (aref virtual-key (node-size node))))
            (if (eq node (tree-root tree))
                (let ((new-root (new-node tree nil)))
                  (setf (aref (node-keys new-root) 0) new-x)
                  (setf (aref (node-ptrs new-root) 0) node)
                  (setf (aref (node-ptrs new-root) 1) new-internal)
                  (setf (node-size new-root) 1)
                  (setf (tree-root tree) new-root))
                (insert-to-parent tree
                                  (find-parent (tree-root tree) node)
                                  new-x
                                  new-internal)))))))

(defun insert (tree x)
  (if (null (tree-root tree))

      (let ((node (new-node tree t)))
        (setf (aref (node-keys node) 0) x)
        (setf (node-size node) 1)
        (setf (tree-root tree) node))

      (multiple-value-bind (node parent) (search-leaf-node tree x)
        (if (< (node-size node) *MAX*)
            (with-accessors ((keys node-keys)
                             (ptrs node-ptrs)
                             (size node-size)) node
              (let ((i 0))
                (loop while (and (< i size) (< (aref keys i) x))
                      do (incf i))
                (loop for j from size downto (1+ i)
                      do (setf (aref keys j) (aref keys (1- j))))
                (setf (aref keys i) x)
                (incf size)
                (setf (aref ptrs      size) (aref ptrs (1- size)))
                (setf (aref ptrs (1- size)) nil)))

            (let ((virtual-node (make-array (1+ *MAX*)
                                            :initial-element nil)))
              (loop for i from 0 below *MAX* do
                (setf (aref virtual-node i)
                      (aref (node-keys node) i)))
              (let ((i 0))
                (loop while (and (< i *MAX*)
                                 (< (aref virtual-node i) x))
                      do (incf i))
                (loop for j from *MAX* downto (1+ i)
                      do (setf (aref virtual-node j)
                               (aref virtual-node (1- j))))
                (setf (aref virtual-node i) x))
              (let ((new-leaf (new-node tree t)))
                (setf (node-size node)
                      (floor (1+ *MAX*) 2))
                (setf (node-size new-leaf)
                      (- (1+ *MAX*) (node-size node)))
                (setf (aref (node-ptrs node) (node-size node))
                      new-leaf)
                (setf (aref (node-ptrs new-leaf) (node-size new-leaf))
                      (aref (node-ptrs node) *MAX*))
                (loop for i from 0 below (node-size node)
                      do (setf (aref (node-keys node) i)
                               (aref virtual-node i)))
                (loop for i from 0 below (node-size new-leaf)
                      for j from (node-size node)
                      do (setf (aref (node-keys new-leaf) i)
                               (aref virtual-node j)))
                (let ((new-x (aref (node-keys new-leaf) 0))) 
                  (if (eq node (tree-root tree))
                      (let ((new-root (new-node tree nil)))
                        (setf (aref (node-keys new-root) 0) new-x)
                        (setf (aref (node-ptrs new-root) 0) node)
                        (setf (aref (node-ptrs new-root) 1) new-leaf)
                        (setf (node-size new-root) 1)
                        (setf (tree-root tree) new-root))
                      (insert-to-parent tree
                                        parent
                                        new-x
                                        new-leaf)))))))))

;; CL-USER> (let ((tree (mita.util.b+tree:make-tree)))
;;            (mita.util.b+tree:insert tree 10)
;;            (mita.util.b+tree:insert tree 11)
;;            (mita.util.b+tree:insert tree -9)
;;            (mita.util.b+tree:insert tree -8)
;;            (mita.util.b+tree:insert tree 0)
;;            (mita.util.b+tree:insert tree -1)
;;            (mita.util.b+tree:insert tree 5)
;;            (mita.util.b+tree:insert tree 4)
;;            (mita.util.b+tree:insert tree 2)
;;            (mita.util.b+tree:insert tree 3)
;;            tree)
;; #S(MITA.UTIL.B+TREE::TREE
;;    :ROOT #<MITA.UTIL.B+TREE::NODE (:ID 1008 :KEYS #(4) :PTRS #(1003 1007)
;;                                    :LEAF-P NIL) {1008C46AD3}>
;;    :NODE-ID 1008
;;    :NODES (#<MITA.UTIL.B+TREE::NODE (:ID 1008 :KEYS #(4) :PTRS #(1003 1007)
;;                                      :LEAF-P NIL) {1008C46AD3}>
;;            #<MITA.UTIL.B+TREE::NODE (:ID 1007 :KEYS #(10) :PTRS #(NIL 1005)
;;                                      :LEAF-P NIL) {1008C469C3}>
;;            #<MITA.UTIL.B+TREE::NODE (:ID 1006 :KEYS #(2 3) :PTRS
;;                                      #(NIL NIL 1005) :LEAF-P T) {1008C46923}>
;;            #<MITA.UTIL.B+TREE::NODE (:ID 1005 :KEYS #(4 5) :PTRS
;;                                      #(NIL NIL 1002) :LEAF-P T) {1008C46853}>
;;            #<MITA.UTIL.B+TREE::NODE (:ID 1004 :KEYS #(-1 0) :PTRS
;;                                      #(NIL NIL 1006) :LEAF-P T) {1008C46783}>
;;            #<MITA.UTIL.B+TREE::NODE (:ID 1003 :KEYS #(-1 2) :PTRS
;;                                      #(1001 1004 1006) :LEAF-P
;;                                      NIL) {1008C466B3}>
;;            #<MITA.UTIL.B+TREE::NODE (:ID 1002 :KEYS #(10 11) :PTRS
;;                                      #(NIL NIL NIL) :LEAF-P T) {1008C46613}>
;;            #<MITA.UTIL.B+TREE::NODE (:ID 1001 :KEYS #(-9 -8) :PTRS
;;                                      #(NIL NIL 1004) :LEAF-P T) {1008C46543}>))

(defun search (tree x)
  (let ((node (search-leaf-node tree x)))
    (let ((keys (node-keys node))
          (size (node-size node)))
      (loop for i from 0 below size
            for key = (aref keys i)
            if (= key x) return node))))


(defun handle-vanish-node (tree parent node)
  (with-accessors ((keys node-keys)
                   (ptrs node-ptrs)
                   (size node-size)) parent
    (let ((pos (position node ptrs)))
      (cond ((= pos 0)
             (loop for i from 0 below (1- size) do
               (setf (aref keys i) (aref keys (1+ i))))
             (loop for j from 0 below size do
               (setf (aref ptrs j) (aref ptrs (1+ j)))))
            ((= pos size)
             (decf size))
            (t
             (setf (aref ptrs i) nil))))))

(defun handle-delete-first-key (tree parent x child)
  (let ((i (position x (node-keys parent)
                     :test #'=
                     :end (node-size parent))))
    (when i
      (setf (aref (node-keys parent) i)
            (aref (node-keys child) 0))
      (when (= i 0)
        (handle-delete-first-key tree
                                 (find-parent (tree-root tree) parent)
                                 x
                                 child)))))

(defun delete (tree x)
  (when (tree-root tree)
    (multiple-value-bind (leaf parent)
        (search-leaf-node tree x)
      (when leaf
        (with-accessors ((keys node-keys)
                         (ptrs node-ptrs)
                         (size node-size)) leaf
          (let ((i (position x keys :test #'= :end size)))
            (when i
              (let ((h 0))
                (loop while (= x (aref keys (+ i h))) do (incf h))
                (cond ((= h size)
                       (let ((ii (position node (node-ptrs parent)
                                           :test #'=
                                           :end (node-size parent))))
                         (setf (aref (node-ptrs parent) ii) nil)
                         (dolist (n (tree-nodes tree))
                           (when (eq (aref (node-ptrs n) (node-size n))
                                     node)
                             (setf (aref (node-ptrs n) (node-size n))
                                   (aref ptrs size)))))
                       (handle-vanish-node tree parent node)
                       (alexandria:deletef (tree-nodes tree) node))
                      (t
                       (loop k from 0 while (< (+ i h k) size) do
                         (setf (aref keys (+ i k))
                               (aref keys (+ i k h))))
                       (setf (aref ptrs (- size h)) (aref ptrs size))
                       (decf size h)
                       (when (= i 0)
                         (handle-delete-first-key
                          tree parent x leaf))))))))))))
