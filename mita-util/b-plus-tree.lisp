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
                (- (1+ *MAX*) (node-size node)))
          (loop for i from 0 below (node-size node)
                do (setf (aref (node-keys node) i)
                         (aref virtual-key i)))
          (loop for i from 0 below (1+ (node-size node))
                do (setf (aref (node-ptrs node) i)
                         (aref virtual-ptr i)))
          (loop for i from 0 below (node-size new-internal)
                for j from (+ 0 (node-size node)) do
            (setf (aref (node-keys new-internal) i)
                  (aref virtual-key j)))
          (loop for i from 1 below (1+ (node-size new-internal))
                for j from (+ 1 (node-size node)) do
            (setf (aref (node-ptrs new-internal) i)
                  (aref virtual-ptr j)))
          (let ((new-x (aref (node-keys new-internal) 0)))
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

(defun search (tree x)
  (let ((node (search-leaf-node tree x)))
    (let ((keys (node-keys node))
          (size (node-size node)))
      (loop for i from 0 below size
            for key = (aref keys i)
            if (= key x) return node))))
