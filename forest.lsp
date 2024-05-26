;class node of tree
;fields: val (integer) - value
;        color (string) - color of node (red/black)
;        children (list node*) - list of children
;        root (node) - parent node
;methods: tored
;         toblack
;         print-object
;         get_left_child
;         get_right_child
;         set_left_child
;         set_right_child
;         set_root
;         tolistik
;         islistik
;         is_root
(defclass node ()
    (
        (val :accessor val :initform -1000)
        (color :accessor color :initform "b")
        (children :accessor children :initform (cons nil nil))
        (root :accessor root :initform nil)
    )
)

(defun create_list ()
    (make-instance 'node)
)

(defun create_node (vl clr &optional rt ch)
    (let* ((new (make-instance 'node)))
        (setf (color new) clr)
        (setf (val new) vl) 
        (if (not (null rt)) (setf (root new) root))
        (if (not (null ch)) (setf (children new) ch) (setf (children new) (cons (create_list) (create_list))))
        new
    )
)


;method tored on node class
;desc: change node color to red
;params: object - src node
;return: node
(defmethod tored ((object node))
   (and (setf (color object) "r") object)
)

(defmethod toroot ((object node))
    (cond
        ((is_root object) object)
        (T (toroot (root object)))
    )
)

;method toblack on node class
;desc: change node color to black
;params: object - src node
;return node
(defmethod toblack ((object node))
   (and (setf (color object) "b") object)
)

;method print-object on node class
;desc: print node in human-readable view
;params: object - src node
(defmethod print-object ((object node) out)
    (print-unreadable-object (object out :type t)
        (format out "color=~s val=~d" (color object) (val object)))
)

;method get_left_child on node class
;desc: get left child of tree node
;params: object - src node
;return: node
(defmethod get_left_child ((object node))
    (car (children object))
)

;method get_right_child on node class
;desc: get right child of tree node
;params: object - src node
;return: node
(defmethod get_right_child ((object node))
    (cdr (children object))
)

(defun get_left_grandson (object)
    (cond 
        ((null object) nil)
        ((islistik object) nil)
        (T (get_left_child (get_left_child object)))
    )
)

(defun get_lr_grandson (object)
    (cond
        ((null object) nil)
        ((islistik object) nil)
        (T (get_right_child (get_left_child object)))
    )
)

(defun get_rl_grandson (object)
    (cond
        ((null object) nil)
        ((islistik object) nil)
        (T (get_left_child (get_right_child object)))
    )
)

(defun get_right_grandson (object)
    (cond 
        ((null object) nil)
        ((islistik object) nil)
        (T (get_right_child (get_right_child object)))
    )
)

;method set_left_child on node class
;desc: set new_val node as left child of parent object node
;params: object - src node
;        new_val - new left child node
;return: node
(defmethod set_left_child ((object node) new_val)
    (cond
        ((null new_val) object)
        ((not (isnullnode (get_left_child object))) object)
        (T     (and 
                    (tolistik new_val)
                    (rplaca (children object) new_val)
                    (setf (root new_val) object)
                    object
                )
        )
    )
)

;method set_right_child on node class
;desc: set new_val node as right child of parent object node
;params: object - src node
;        new_val - new right child node
;return: node
(defmethod set_right_child ((object node) new_val)
    (cond
        ((null new_val) object)
        ((not (isnullnode (get_right_child object))) object)
        (T     (and 
                    (tolistik new_val)
                    (setf (cdr (children object)) new_val)
                    (setf (root new_val) object)
                    object
                )
        )
    )
)

;method tolistik on node class
;desc: make list from tree node
;params: object - src node
;return: node
(defmethod tolistik ((object node))
    (and (rplaca (children object) (create_list)) 
         (rplacd (children object) (create_list)) 
         object
    )
)

;method islistik on node class
;desc: list check of node
;params: object - src node
;return: T - object is tree list
;        Nil - object isnt tree list
(defmethod islistik ((object node))
    (and (= (val (car (children object))) -1000) (= (val (cdr (children object))) -1000))
)

(defmethod isnullnode ((object node))
    (and (= (val object) -1000) (equalp (children object) (cons nil nil)))
)

;method is_root on node class
;desc: root check of node
;params: object - src node
;return: T - object is tree root
;        Nil - object isnt tree root
(defmethod is_root ((object node))
    (null (root object))
)

;method insert
;desc: insert node to binary rb tree
;params: root - node that is root of tree
;        item - insert node
;return: node
(defun insert (root item)
    (cond ((null root) item)
          ((and (isnullnode  (get_left_child root)) (< (val item) (val root))) (set_left_child root item))
          ((and (< (val item) (val root))) (insert (car (children root)) item))
          ((and (isnullnode (get_right_child root)) (> (val item) (val root))) (set_right_child root item))
          ((and (> (val item) (val root))) (insert (cdr (children root)) item))
    )
)

;method left_turn
;desc: make a large left turn of the tree
;params: parent - node around which the rotation is made
;return: node
(defun left_turn (parent) 
    (let* (
            (tmp_parent (copy-tree parent))
            (tmp_left_son (copy-tree (get_left_child parent)))
            (tmp_right_son (copy-tree (get_right_child parent)))
            (tmp_rl_grand (copy-tree (get_rl_grandson parent)))
            (tmp_rr_grand (copy-tree (get_right_grandson parent)))

            (right_son (copy-tree (get_right_child parent)))
            (left_son   (copy-tree (get_left_child parent)))
            (rl_grand     (copy-tree (get_rl_grandson parent)))
            (rr_grand    (copy-tree (get_right_grandson parent)))
          )

            (setf (root rl_grand) parent)
            (rplacd (children parent) rl_grand)

            (setf (root right_son) (root tmp_parent))
            
            (cond ((is_root tmp_parent) right_son)
                  ((equalp (get_left_child (root tmp_parent)) tmp_parent) (setf (car (children (root tmp_parent))) right_son))
                  (T (rplacd (children (root tmp_parent)) right_son)))

            (setf (root parent) right_son)
            (rplaca (children right_son) parent)

            right_son
    )
)

;mathod right turn
;desc: make a large right turn of the tree
;params: parent - node around which the rotation is made
;return: node
(defun right_turn (parent)
    (let* (
            (tmp_parent (copy-tree parent))
            (tmp_left_son (copy-tree (get_left_child parent)))
            (tmp_right_son (copy-tree (get_right_child parent)))
            (tmp_ll_grand (copy-tree (get_left_grandson parent)))
            (tmp_lr_grand (copy-tree (get_lr_grandson parent)))

            (right_son (copy-tree (get_right_child parent)))
            (left_son   (copy-tree (get_left_child parent)))
            (ll_grand     (copy-tree (get_left_grandson parent)))
            (lr_grand    (copy-tree (get_lr_grandson parent)))
        )

            (setf (root lr_grand) parent)
            (setf (car (children parent)) lr_grand)

            (setf (root left_son) (root tmp_parent))
            
            (cond ((is_root tmp_parent) left_son)
                  ((equalp (get_left_child (root tmp_parent)) tmp_parent) (setf (car (children (root tmp_parent))) left_son))
                  (T (setf (cdr (children (root tmp_parent))) left_son)))

            (setf (root parent) left_son)
            (setf (cdr (children left_son)) parent)

            left_son
    )
)

;method small_left_turn
;desc: make a small left turn of the tree
;params: parent - node around which the rotation is made
;return: node
(defun small_left_turn (parent)
    (let*   (
                (left_child     (copy-tree (get_left_child parent)))
                (right_grandson (copy-tree (get_right_child left_child)))
                (left_grandson  (copy-tree (get_left_child left_child)))
            )
        
        (set_right_child left_child nil)
        (set_left_child right_grandson left_child)
        (set_left_child parent right_grandson)
        parent
        )
)

;method small_right_turn
;desc: make a small right turn of the tree
;params: parent - node around which the rotation is made
;return: node
(defun small_right_turn (parent)
    (let* (
                (right_child   (copy-tree (get_right_child parent)))
                (left_grandson     (copy-tree (get_left_child right_child)))
          )

        (set_left_child right_child nil)
        (set_right_child left_grandson right_child)
        (set_right_child parent left_grandson)
        parent
    )
)

;method swap_color_red_up
;desc: make a swap of colors when there is a red node at the top
;params: parent - parent node of swapping
;return: node
(defun swap_color_red_up (parent)
    (let* ((cur_parent  parent)
           (left (get_left_child parent))
           (right (get_right_child parent))
           )

           (tored cur_parent)
           (toblack left)
           (toblack right)
            parent
    )
)

;method swap_color_black_up
;desc: make a swap of colors when there is a black node at the top
;params: parent - parent node of swapping
;return: node
(defun swap_color_black_up (parent)
    (let* ((cur_parent (car parent))
           (left (caadr parent))
           (right (cadadr parent))
           )

           (toblack cur_parent)
           (tored left)
           (tored right)
            parent
    )
)

;method print_tree
;desc: recursive print of tree
;params: parent - root of tree
(defun print_tree (parent)
    (cond ((null parent) 1)
          ((isnullnode parent) 1)
          ((islistik parent) (print parent))
          (T (and (print parent) (print (children parent)) (print_tree (car (children parent))) (print_tree (cdr (children parent)))))
    )
)

;method case1
;desc: null case for balancing
;params: parent - current root of tree
;return: T - null case was realized
;        Nil - null case wasnt realized
(defun case0 (parent)
    (and
        (not (null parent))
        (is_root parent)
        (equalp "r" (color parent)) 
    )
)

;method case1
;desc: first case for balancing
;params: parent - current root of tree
;return: T - first case was realized
;        Nil - first case wasnt realized
(defun case1 (parent)
    (let* ((son1        (get_left_child parent))
           (uncle_son2    (get_right_child parent))
           (grandson  (get_left_grandson parent)))
            (print "1-------")
            (print son1)
            (print grandson)
            (print uncle_son2)
            (print "1-------")
          (and (not (null grandson))
               (equal "r" (color son1))
               (equal "r" (color uncle_son2))
               (equal "r" (color grandson)))
    )
)

;method case1_right
;desc: first case for balancing
;params: parent - current root of tree
;return: T - first case was realized
;        Nil - first case wasnt realized
(defun case1_right (parent)
    (let* ((son1        (get_left_child parent))
           (uncle_son2    (get_right_child parent))
           (grandson  (get_right_grandson parent)))
            (print "-------")
            (print son1)
            (print grandson)
            (print uncle_son2)
            (print "-------")
          (and (not (null grandson))
               (equal "r" (color son1))
               (equal "r" (color uncle_son2))
               (equal "r" (color grandson)))
    )
)

;method case2_left
;desc: second left case for balancing
;params: parent - current root of tree
;return: T - second left case was realized
;        Nil - second left case wasnt realized
(defun case2_left (parent)
    (let* ((uncle_son2         (get_right_child parent))
           (grandson    (get_lr_grandson parent))
           (son1    (get_left_child parent)))
            
            (print "-------")
            (print son1)
            (print grandson)
            (print uncle_son2)
            (print "-------")

          (and (not (null grandson))
               (equal "b" (color uncle_son2))
               (equal "r"   (color son1))
               (equal "r"   (color grandson)))
    )
)

;method case2_right
;desc: second right case for balancing
;params: parent - current root of tree
;return: T - second right case was realized
;        Nil - second right case wasnt realized
(defun case2_right (parent)
    (let* ((son1        (get_right_child parent))
           (grandson    (get_rl_grandson parent))
           (uncle_son2  (get_left_child parent)))
            
            (print "-------")
            (print son1)
            (print grandson)
            (print uncle_son2)
            (print "-------")

          (and (not (null grandson))
               (equal "b" (color uncle_son2))
               (equal "r"   (color son1))
               (equal "r"   (color grandson)))
    )
)

;method case3_left
;desc: third left case for balancing
;params: parent - current root of tree
;return: T - third left case was realized
;        Nil - third left case wasnt realized
(defun case3_left (parent)
    (let* ((son1          (get_left_child parent))
           (uncle_son2    (get_right_child parent))
           (grandson      (get_left_grandson parent))
           )

            (print "-------")
            (print son1)
            (print grandson)
            (print uncle_son2)
            (print "-------")

          (and (not (null grandson))
               (equal "r" (color son1))
               (equal "b" (color uncle_son2))
               (equal "r" (color grandson)))
    )
)

;method case3_right
;desc: third right case for balancing
;params: parent - current root of tree
;return: T - third right case was realized
;        Nil - third right case wasnt realized
(defun case3_right (parent)
    (let* ((son1        (get_right_child parent))
           (uncle_son2  (get_left_child parent))
           (grandson    (get_right_grandson parent)))

            (print "-------")
            (print son1)
            (print grandson)
            (print uncle_son2)
            (print "-------")

          (and (not (null grandson))
               (equal "r" (color son1))
               (equal "b" (color uncle_son2))
               (equal "r" (color grandson)))
    )
)

(defun case0_impl (parent)
    (toblack parent)
)

;method case1_impl
;desc: balancing in the first case
;params: parent - current root of tree
;return: node
(defun case1_impl_left (parent)
    (cond ((is_root parent) (and (swap_color_red_up parent) (toblack parent)))
          (T (and (swap_color_red_up parent) (balance_tree (root parent))))
    )
)

(defun case1_impl_right (parent)
    (cond ((is_root parent) (and (swap_color_red_up parent) (toblack parent)))
          (T (and (swap_color_red_up parent) (balance_tree (root parent))))
    )
)

;method case2_impl_left
;desc: balancing in the second left case
;params: parent - current root of tree
;return: node
(defun case2_impl_left (parent)
    (let* ((root_of_parent (root parent)))
        (cond ((is_root parent)  (toblack (right_turn (small_left_turn parent))))
            (T (and (left_turn (get_left_child parent)) (right_turn parent)
                    (toblack (get_left_child root_of_parent))
                    (tored (get_lr_grandson root_of_parent))
                    (balance_tree root_of_parent)
                )))
    )
)

;method case2_impl_right
;desc: balancing in the second right case
;params: parent - current root of tree
;return: node
(defun case2_impl_right (parent)
    (let* ((root_of_parent (root parent)))
        (cond ((is_root parent) (toblack (left_turn (small_right_turn parent))))
            (T (and (right_turn (get_right_child parent)) (left_turn parent) 
                    (toblack (get_right_child root_of_parent))
                    (tored (get_rl_grandson root_of_parent))
                    (balance_tree root_of_parent)
                )))
    )
)

;method case3_impl_left
;desc: balancing in the third left case
;params: parent - current root of tree
;return: node
(defun case3_impl_left (parent)
    (let* ((root_of_parent (root parent)))
        (cond ((is_root parent) (toblack (left_turn parent)))
            (T (and (toblack (get_right_child parent))
                    (tored parent)
                    (left_turn parent) 
                    T
                    (balance_tree root_of_parent)
                )))
    )
)

;method case3_impl_right
;desc: balancing in the third right case
;params: parent - current root of tree
;return: node
(defun case3_impl_right (parent)
    (let* ((root_of_parent (root parent)))
        (cond ((is_root parent) (toblack (right_turn parent)))
            (T (and (toblack (get_left_child parent))
                    (tored parent)
                    (right_turn parent) 
                    T
                    (balance_tree root_of_parent)
                )))
    )
)

;method balance_tree
;desc: recursive balancing of rb tree
;params: parent - initial node of balancing
;return: node
(defun balance_tree (parent)
    (cond 
            ((null parent)                            (and (print "nil") nil))
            ((isnullnode parent)                      (and (print "nullnode") (balance_tree (root parent))))
            ((and (islistik parent) (not (is_root parent)))                        (and (print "list") (balance_tree (root parent))))
            ((case0 parent)                           (and (print "0") (case0_impl parent)))
            ((case1 parent)                           (and (print "1.1") (case1_impl_left parent)))
            ((case1_right parent)                     (and (print "1.2") (print parent) (case1_impl_right parent)))
            ((case2_left parent)                      (and (print "2.1") (case2_impl_left parent)))
            ((case2_right parent)                     (and (print "2.2") (case2_impl_right parent)))
            ((case3_left parent)                      (and (print "3.1") (case3_impl_right parent)))
            ((case3_right parent)                     (and (print "3.2") (case3_impl_left parent)))
            (t                                        (and (print "4 - balanced") (balance_tree (root parent))))
    )
)

;method depth_trav
;desc: recursive depth-first traversal with balancing
;params: parent - root node of tree
(defun depth_trav (parent)
    (cond 
            ((null parent) 1)
            ((islistik parent) (balance_tree (root (root parent))))
            (T (depth_trav (and (balance_tree (car (children parent))) (balance_tree (cadr (children parent))))))
    )
)

;method merge_tree
;desc: merging to rb binary trees
;params: src - root of source tree
;        dest - root of destination tree
(defun merge_tree (src dst)
    (cond ((null src) 1)
          ((islistik src) (and (insert dst (tored src)) (balance_tree dst)))
          (T (and   (and (insert dst (tored src)) 
                         (balance_tree dst))
                  (merge_tree (car (children src)) dst) 
                  (merge_tree (cadr (children src)) dst)))
    )
)




