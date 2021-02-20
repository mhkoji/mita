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


(defun find-parent (node child)
  (when (not (node-leaf-p node))
    (loop for i from 0 to (node-size node)
          for child-node = (aref (node-ptrs node) i)
          when child-node do
      (progn
        (when (eq child-node child)
          (return-from find-parent node))
        (let ((parent (find-parent child-node child)))
          (when parent
            (return-from find-parent parent)))))))


(defun search-leaf-node-to-insert (tree x)
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

(defun insert-child (tree node x child)
  (assert (and (< 0 (length (node-keys child)))
               (let ((first-key (aref (node-keys child) 0)))
                 (or (< x first-key) (= x first-key)))))
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
          ;;
          ;; Try to insert (43, p), where *MAX* = 3, to the following:
          ;;
          ;;                   30   40   50
          ;;                 x    y    z    w
          ;;
          ;; First, prepare some intermediate arrays:
          ;;
          ;;                    0        1        2 (= i)    3
          ;;
          ;; virtual-keys:     30       40       43         50
          ;;
          ;; virtual-ptrs:  x       y        z        p             w
          ;;
          ;;                0       1        2        3 (= (1+ i)   4
          ;;
          (loop while (and (< i *MAX*)
                           (< (aref virtual-key i) x))
                do (incf i))
          (loop for j from (+ *MAX* 0) downto (+ i 1)
                do (setf (aref virtual-key j) (aref virtual-key (1- j))))
          (setf (aref virtual-key i) x)
          (loop for j from (+ *MAX* 1) downto (+ i 2)
                do (setf (aref virtual-ptr j) (aref virtual-ptr (1- j))))
          (setf (aref virtual-ptr (1+ i)) child)
          ;;
          ;; Then, make the resulting nodes:
          ;;
          ;;
          ;;        new-x:          +------------43---------+
          ;;                        |                       |
          ;;     new-node:     30       40                  |
          ;;                 x      y        z              |
          ;;                                                |
          ;; new-internal:                                  50
          ;;                                           p           w
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
          (loop for i from 0 below (1+ (node-size new-internal))
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
                (let ((parent (find-parent (tree-root tree) node)))
                  (insert-child tree parent new-x new-internal))))))))

;; https://www.geeksforgeeks.org/insertion-in-a-b-tree/
(defun insert (tree x)
  (if (null (tree-root tree))

      (let ((node (new-node tree t)))
        (setf (aref (node-keys node) 0) x)
        (setf (node-size node) 1)
        (setf (tree-root tree) node))

      (multiple-value-bind (node parent)
          (search-leaf-node-to-insert tree x)
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
                      (insert-child tree parent new-x new-leaf)))))))))


(defun search (tree x)
  (let ((node (search-leaf-node-to-insert tree x)))
    (let ((keys (node-keys node))
          (size (node-size node)))
      (loop for i from 0 below size
            for key = (aref keys i)
            if (= key x) return node))))


(defun search-sequence-to-leaf (tree x)
  (labels ((iter (node acc)
             (with-accessors ((keys node-keys)
                              (ptrs node-ptrs)
                              (size node-size)) node
               (if (node-leaf-p node)
                   (loop for i from 0 below size
                         for key = (aref keys i)
                         when (= key x)
                           return (cons node acc))
                   (loop for i from 0 below size
                         for key = (aref keys i)
                         when (or (< x key))
                           return (iter (aref ptrs i)
                                        (cons node acc))
                         when (= i (1- size))
                           return (iter (aref ptrs (1+ i))
                                        (cons node acc)))))))
    (iter (tree-root tree) nil)))

(defun delete-child (tree child parents)
  (let ((node (car parents)))
    (let ((i (position child (node-ptrs node)
                       :test #'eq
                       :end (1+ (node-size node)))))
      (assert i)
      (cond ((= i (node-size node))
             (setf (aref (node-ptrs node) (node-size node)) nil)
             (decf (node-size node)))
            (t
             (loop for k from i below (1- (node-size node)) do
               (setf (aref (node-keys node) k)
                     (aref (node-keys node) (1+ k))))
             (loop for k from i below (node-size node) do
               (setf (aref (node-ptrs node) k)
                     (aref (node-ptrs node) (1+ k))))
             (decf (node-size node))))
      (alexandria:deletef (tree-nodes tree) child))

    (when (< (node-size node) (floor *MAX* 2))
      (if (eq node (tree-root tree))
          (when (= (node-size node) 0)
            ;; The node has only one child, which should be the new root
            (setf (tree-root tree)
                  (or (aref (node-ptrs node) 0)
                      (aref (node-ptrs node) 1)))
            (alexandria:deletef (tree-nodes tree) node))
          (let* ((parent (cadr parents))
                 (parent-ptrs (node-ptrs parent))
                 (j (position node parent-ptrs
                              :test #'eq
                              :end (1+ (node-size parent))))
                 (left (when (<= 0 (1- j))
                         (aref parent-ptrs (1- j))))
                 (right (when (<= (1+ j) (node-size parent))
                          (aref parent-ptrs (1+ j)))))
            (cond ((and right
                        (<= (floor *MAX* 2) (1- (node-size right))))
                   (progn ;; extend node
                     (setf (aref (node-keys node) (node-size node))
                           (aref (node-keys right) 0))
                     (incf (node-size node))
                     (setf (aref (node-ptrs node) (node-size node))
                           (aref (node-ptrs right) 0)))
                   (progn ;; shrink right
                     (loop for k from 0 below (1- (node-size right))
                           do (setf (aref (node-keys right) k)
                                    (aref (node-keys right) (1+ k))))
                     (loop for k from 0 below (node-size right)
                           do (setf (aref (node-ptrs right) k)
                                    (aref (node-ptrs right) (1+ k))))
                     (decf (node-size right)))
                   (setf (aref (node-keys parent) j)
                         (aref (node-keys right) 0)))
                  ((and left
                        (<= (floor *MAX* 2) (1- (node-size left))))
                   (progn ;; extend node
                     (loop for k from (1- (node-size node)) downto 1
                           do (setf (aref (node-keys node) k)
                                    (aref (node-keys node) (1- k))))
                     (setf (aref (node-keys node) 0)
                           (aref (node-keys left) (1- (node-size left))))
                     (loop for k from (node-size node) downto 1
                           do (setf (aref (node-ptrs node) k)
                                    (aref (node-ptrs node) (1- k))))
                     (setf (aref (node-ptrs node) 0)
                           (aref (node-ptrs left)
                                 (node-size left)))
                     (incf (node-size node)))
                   (progn ;; shring left
                     (setf (aref (node-ptrs left) (node-size left))
                           nil)
                     (decf (node-size left)))
                   #+nil
                   (setf (aref (node-keys parent) j)
                         (aref (node-keys right) 0)))
                  (right
                   ;; node <- node + parent-key + right
                   (setf (aref (node-keys node) (node-size node))
                         (aref (node-keys parent) 0))
                   (incf (node-size node))
                   (loop for i from 0 below (node-size right)
                         for x = (aref (node-keys right) i)
                         for j from (node-size node)
                         do (setf (aref (node-keys node) j) x))
                   (loop for i from 0 below (1+ (node-size right))
                         for p = (aref (node-ptrs right) i)
                         for j from (node-size node)
                         do (setf (aref (node-ptrs node) j) p))
                   (setf (node-size node) (+ (node-size node)
                                             (node-size right)))
                   (delete-child tree right (cddr parents)))
                  (t
                   (assert left)
                   ;; left <- left + node
                   (loop for i from 0 below (node-size node)
                         for j from (node-size left)
                         do (let ((x (if (and (= i 0)
                                              (aref (node-ptrs node) 0))
                                         (aref (node-keys
                                                (aref (node-ptrs node) 0))
                                               0)
                                         (aref (node-keys node) i)) ))
                              (setf (aref (node-keys left) j) x)))
                   (loop for i from 1 below (1+ (node-size node))
                         for p = (aref (node-ptrs node) i)
                         for j from (1+ (node-size left))
                         do (setf (aref (node-ptrs left) j) p))
                   (setf (node-size left) (+ (node-size left)
                                             (node-size node)))
                   (delete-child tree node (cddr parents))
                   (setf (car parents) left))))))))

;; https://www.youtube.com/watch?v=pGOdeCpuwpI
(defun delete (tree x)
  (when (tree-root tree)
    (let ((nodes (search-sequence-to-leaf tree x)))
      (when nodes
        (let ((leaf (car nodes)))
          (let ((i (position x (node-keys leaf)
                             :test #'=
                             :end (node-size leaf))))
            (loop for k from i below (1- (node-size leaf))
                  do (setf (aref (node-keys leaf) k)
                           (aref (node-keys leaf) (1+ k))))
            (with-accessors ((ptrs node-ptrs)
                             (size node-size)) leaf
              (setf (aref ptrs (1- size)) (aref ptrs size))
              (setf (aref ptrs      size) nil)
              (decf size)))
          (when (and (< (node-size leaf)
                        (floor *MAX* 2))
                     (not (eq leaf (tree-root tree))))
            (assert (cadr nodes))

            ;; should merge
            (let* ((parent (cadr nodes))
                   (parent-ptrs (node-ptrs parent))
                   (j (position leaf parent-ptrs
                                :test #'eq
                                :end (1+ (node-size parent))))
                   (left (when (<= 0 (1- j))
                           (aref parent-ptrs (1- j))))
                   (right (when (<= (1+ j) (node-size parent))
                            (aref parent-ptrs (1+ j)))))
              (cond ((and right
                          (<= (floor *MAX* 2) (1- (node-size right))))
                     (progn ;; extend leaf
                       (setf (aref (node-keys leaf) (node-size leaf))
                             (aref (node-keys right) 0))
                       (setf (aref (node-ptrs leaf) (1+ (node-size leaf)))
                             (aref (node-ptrs leaf) (node-size leaf)))
                       (incf (node-size leaf)))
                     (progn ;; shrink right
                       (loop for k from 0 below (1- (node-size right))
                             do (setf (aref (node-keys right) k)
                                      (aref (node-keys right) (1+ k))))
                       (setf (aref (node-ptrs right) (node-size right))
                             (aref (node-ptrs right) (1- (node-size right))))
                       (decf (node-size right)))
                     (setf (aref (node-keys parent) j)
                           (aref (node-keys right) 0)))
                    ((and left
                          (<= (floor *MAX* 2) (1- (node-size left))))
                     (let ((borrowed-key ;; extend leaf
                             (aref (node-keys left)
                                   (1- (node-size left)))))
                       (loop for k from (1- (node-size leaf)) downto 1
                             do (setf (aref (node-keys leaf) k)
                                      (aref (node-keys leaf) (1- k))))
                       (setf (aref (node-keys leaf) 0)
                             borrowed-key)
                       (setf (aref (node-ptrs leaf) (1+ (node-size leaf)))
                             (aref (node-ptrs leaf) (node-size leaf)))
                       (setf (aref (node-ptrs leaf) (node-size leaf))
                             nil)
                       (incf (node-size leaf)))
                     (progn ;; shrink left
                       (setf (aref (node-ptrs left) (1- (node-size left)))
                             (aref (node-ptrs left) (node-size left)))
                       (decf (node-size left)))
                     (setf (aref (node-keys parent) (1- j))
                           (aref (node-keys leaf) 0)))
                    (right
                     ;; leaf <- leaf + right
                     (loop for i from 0 below (node-size right)
                           for x = (aref (node-keys right) i)
                           for j from (node-size leaf)
                           do (setf (aref (node-keys leaf) j) x))
                     (setf (aref (node-ptrs leaf) (node-size leaf))
                           nil)
                     (setf (node-size leaf) (+ (node-size leaf)
                                               (node-size right)))
                     (setf (aref (node-ptrs leaf) (node-size leaf))
                           (aref (node-ptrs right) (node-size right)))
                     (delete-child tree right (cdr nodes)))
                    (t
                     (assert left)
                     ;; left <- left + leaf
                     (loop for i from 0 below (node-size leaf)
                           for x = (aref (node-keys leaf) i)
                           for j from (node-size left)
                           do (setf (aref (node-keys left) j) x))
                     (setf (node-size left) (+ (node-size left)
                                               (node-size leaf)))
                     (setf (aref (node-ptrs left) (node-size left))
                           (aref (node-ptrs leaf) (node-size leaf)))
                     (delete-child tree leaf (cdr nodes))))))
          (dolist (node (cdr nodes))
            ;; skip (car nodes) = leaf
            (let ((i (position x (node-keys node)
                               :test #'=
                               :end (node-size node))))
              (when i
                (setf (aref (node-keys node) i)
                      (aref (node-keys leaf) 0))))))))))

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
;;                                    :LEAF-P NIL) {100881F773}>
;;    :NODE-ID 1008
;;    :NODES (#<MITA.UTIL.B+TREE::NODE (:ID 1008 :KEYS #(4) :PTRS #(1003 1007)
;;                                      :LEAF-P NIL) {100881F773}>
;;            #<MITA.UTIL.B+TREE::NODE (:ID 1007 :KEYS #(10) :PTRS #(1005 1002)
;;                                      :LEAF-P NIL) {100881F663}>
;;            #<MITA.UTIL.B+TREE::NODE (:ID 1006 :KEYS #(2 3) :PTRS
;;                                      #(NIL NIL 1005) :LEAF-P T) {100881F5C3}>
;;            #<MITA.UTIL.B+TREE::NODE (:ID 1005 :KEYS #(4 5) :PTRS
;;                                      #(NIL NIL 1002) :LEAF-P T) {100881F4F3}>
;;            #<MITA.UTIL.B+TREE::NODE (:ID 1004 :KEYS #(-1 0) :PTRS
;;                                      #(NIL NIL 1006) :LEAF-P T) {100881F423}>
;;            #<MITA.UTIL.B+TREE::NODE (:ID 1003 :KEYS #(-1 2) :PTRS
;;                                      #(1001 1004 1006) :LEAF-P
;;                                      NIL) {100881F353}>
;;            #<MITA.UTIL.B+TREE::NODE (:ID 1002 :KEYS #(10 11) :PTRS
;;                                      #(NIL NIL NIL) :LEAF-P T) {100881F2B3}>
;;            #<MITA.UTIL.B+TREE::NODE (:ID 1001 :KEYS #(-9 -8) :PTRS
;;                                      #(NIL NIL 1004) :LEAF-P T) {100881F1E3}>))
