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
        (root :accessor root :initform (cons nil nil))
    )
)

;method create_list
;desc: create rb listik
;return: node
(defun create_list ()
    (make-instance 'node)
)

;method create_node
;desc: create rb tree node
;params: vl - value
;        clr - color
;        (opt) rt - root of node
;        (opt) ch - children of node
;return: node
(defun create_node (vl clr &optional rt ch)
    (let* ((new (make-instance 'node)))
        (setf (color new) clr)
        (setf (val new) vl) 
        (if (not (null rt)) (setf (root new) (cons rt nil)))
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

;method toroot on node class
;desc: returns root of rb tree
;params: object - src node
;return: node
(defmethod toroot ((object node))
    (cond
        ((is_root object) object)
        (T (toroot (car (root object))))
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

;method get_left_grandson
;desc: get left son of left son
;params: object - src node
;return: node
(defun get_left_grandson (object)
    (cond 
        ((null object) nil)
        ((islistik object) nil)
        (T (get_left_child (get_left_child object)))
    )
)

;method get_lr_grandson
;desc: get right son of left son
;params: object - src node
;return: node
(defun get_lr_grandson (object)
    (cond
        ((null object) nil)
        ((islistik object) nil)
        (T (get_right_child (get_left_child object)))
    )
)

;method get_rl_grandson
;desc: get left son of right son
;params: object - src node
;return: node
(defun get_rl_grandson (object)
    (cond
        ((null object) nil)
        ((islistik object) nil)
        (T (get_left_child (get_right_child object)))
    )
)

;method get_right_son
;desc: get right son of right son
;params: object - src node
;return: node
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
                    (rplaca (root new_val) object)
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
                    (rplacd (children object) new_val)
                    (rplaca (root new_val) object)
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

;method isnullnode on node class
;desc: check if node is null rb tree node
;params: object - src node
;return: T - object is is null rb tree node
;        Nil - object isnt is null rb tree node
(defmethod isnullnode ((object node))
    (and (= (val object) -1000) (equalp (children object) (cons nil nil)))
)

;method is_root on node class
;desc: root check of node
;params: object - src node
;return: T - object is tree root
;        Nil - object isnt tree root
(defmethod is_root ((object node))
    (null (car (root object)))
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

            (rplaca (root rl_grand) parent)

            (rplacd (children parent) rl_grand)
            (rplaca (root right_son) (car (root tmp_parent)))
            
            (cond ((is_root tmp_parent) right_son)
                  ((equalp (get_left_child (car (root tmp_parent))) tmp_parent) (rplaca (children (car (root tmp_parent))) right_son))
                  (T (rplacd (children (car (root tmp_parent))) right_son)))

            (rplaca (root parent) right_son)
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

            (rplaca (root lr_grand) parent)
            (rplaca (children parent) lr_grand)

            (rplaca (root left_son) (car (root tmp_parent)))
            
            (cond ((is_root tmp_parent) left_son)
                  ((equalp (get_left_child (car (root tmp_parent))) tmp_parent) (rplaca (children (car (root tmp_parent))) left_son))
                  (T (rplacd (children (car (root tmp_parent))) left_son)))

            (rplaca (root parent) left_son)
            (rplacd (children left_son) parent)

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

;method case0
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

;method case0_impl
;desc: balancing in the null case
;params: parent - current root of tree
;return: node
(defun case0_impl (parent)
    (toblack parent)
)

;method case1_impl_left
;desc: balancing in the first case
;params: parent - current root of tree
;return: node
(defun case1_impl_left (parent)
    (cond ((is_root parent) (and (swap_color_red_up parent) (toblack parent)))
          (T (and (swap_color_red_up parent) (balance_tree (car (root parent)))))
    )
)

;method case1_impl_right
;desc: balancing in the first case
;params: parent - current root of tree
;return: node
(defun case1_impl_right (parent)
    (cond ((is_root parent) (and (swap_color_red_up parent) (toblack parent)))
          (T (and (swap_color_red_up parent) (balance_tree (car (root parent)))))
    )
)

;method case2_impl_left
;desc: balancing in the second left case
;params: parent - current root of tree
;return: node
(defun case2_impl_left (parent)
    (let* ((root_of_parent (car (root parent))))
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
    (let* ((root_of_parent (car (root parent))))
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
    (let* ((root_of_parent (car (root parent))))
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
    (let* ((root_of_parent (car (root parent))))
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
            ((null parent)                            (and (print "nil") T))
            ((isnullnode parent)                      (and (print "nullnode") (balance_tree (car (root parent)))))
            ((and (islistik parent) (not (is_root parent)))                        (and (print "list") (balance_tree (car (root parent)))))
            ((case0 parent)                           (and (print "0") (case0_impl parent)))
            ((case1 parent)                           (and (print "1.1") (case1_impl_left parent)))
            ((case1_right parent)                     (and (print "1.2") (print parent) (case1_impl_right parent)))
            ((case2_left parent)                      (and (print "2.1") (case2_impl_left parent)))
            ((case2_right parent)                     (and (print "2.2") (case2_impl_right parent)))
            ((case3_left parent)                      (and (print "3.1") (case3_impl_right parent)))
            ((case3_right parent)                     (and (print "3.2") (case3_impl_left parent)))
            (t                                        (and (print "4 - balanced") (balance_tree (car (root parent)))))
    )
)

;method tolist
;desc: rb tree to multi-level list
;params: parent - root of rb tree
;        res - result multi-level list
(defun tolist (parent res)
    (cond ((null parent) Nil)
          ((isnullnode parent) Nil)
          ((islistik parent) parent)
          ((isnullnode (get_right_child parent))   (list parent (tolist (car (children parent)) res)))
          ((isnullnode (get_left_child parent))    (list parent (tolist (cdr (children parent)) res)))
          (t (list parent (tolist (car (children parent)) res) (tolist (cdr (children parent)) res)))
    )
)

;method flatten
;desc: multi-level list to single-level list
;params: w - source list
;        ac - tail
(defun flatten (w &optional ac) 
  (cond ((null w) ac)
        ((atom w) (cons w ac))
        ((flatten (car w) (flatten (cdr w) ac)))))

;method merge_tree_from_list
;desc: merging to rb binary trees
;params: src - list with nodes
;        dest - root of destination tree
(defun merge_tree_from_list (src dst)
    (cond ((null src) T)
          (T (and
                    (insert dst (tored (car src)))
                    (balance_tree (car src))
                    (merge_tree_from_list (cdr src) (toroot dst)) 
          ))
    )
)

;method merge_tree
;desc: merging to rb binary trees
;params: src - root of source tree
;        dest - root of destination tree
(defun merge_tree (src dst)
    (let* ((lst (flatten (tolist src Nil))))
        (print lst)
        (merge_tree_from_list lst dst)
    )
)