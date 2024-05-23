(defclass node ()
    (
        (val :accessor val)
        (color :accessor color)
        (children :accessor children)
        (root :accessor root)
    )
)

(defmethod tored ((object node))
   (and (setf (color object) "red") object)
)

(defmethod toblack ((object node))
   (and (setf (color object) "black") object)
)

(defmethod print-object ((object node) out)
    (print-unreadable-object (object out :type t)
        (format out "color=~s val=~d" (color object) (val object)))
)

(defmethod get_left_child ((object node))
    (car (children object))
)

(defmethod get_right_child ((object node))
    (cadr (children object))
)

(defmethod set_left_child ((object node) new_val)
    (rplaca (children object) new_val)
)

(defmethod set_right_child ((object node) new_val)
    (rplacd (children object) (list new_val))
)

(defmethod set_root ((object node) parent)
    (and (setf (root object) parent) object)
)

(defmethod tolistik ((object node))
    (and (rplaca (children object) nil) (rplacd (children object) nil))
)

(defmethod islistik ((object node))
    (and (null (car (children object))) (null (cadr (children object))))
)

(defmethod is_root ((object node))
    (null (root object))
)

(defun insert (root item)
    (cond ((and (< (val item) (val root)) (null (car (children root)))) (and (set_left_child root item) (set_root item root)))
          ((and (< (val item) (val root))) (insert (car (children root)) item))
          ((and (> (val item) (val root)) (null (cadr (children root)))) (and (set_right_child root item) (set_root item root)))
          ((and (> (val item) (val root))) (insert (cadr (children root)) item))
    )
)

(defun left_turn (parent) 
    (let* (
            (new_parent (copy-tree (get_right_child parent)))
            (child1     (tored (copy-tree parent)))
            (child2     (copy-tree (get_right_child (get_right_child parent))))
            (child11    (copy-tree (get_left_child parent)))
            (child12    (copy-tree (get_left_child new_parent)))
          )
            ; (print new_parent)
            ; (print child1)
            ; (print child2)
            ; (print child11)
            ; (print "---")
            ; (print child12)

            (set_left_child new_parent child1)
            (set_right_child new_parent child2)
            (set_left_child child1 child11)
            (set_right_child child1 child12)
            new_parent
    )
)


(defun right_turn (parent)
    (let* (
            (new_parent     (copy-tree (get_left_child parent)))
            (child1         (copy-tree (get_left_child (get_left_child parent))))
            (child2         (tored (copy-tree parent)))
            (child22        (copy-tree (get_right_child parent)))
            (child21        (copy-tree (cadr (children (get_left_child parent)))))
        )
        ; (print new_parent)
        ; (print child1)
        ; (print child2)
        ; (print child21)
        ; (print child22)

        (set_left_child new_parent child1)
        (set_right_child new_parent child2)
        (set_left_child child2 child21)
        (set_right_child child2 child22)
        new_parent
    )
)

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

(defun swap_color_red_up (parent)
    (let* ((cur_parent (car parent))
           (left (caadr parent))
           (right (cadadr parent))
           )

           (tored cur_parent)
           (toblack left)
           (toblack right)
            parent
    )
)

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

(defun print_tree (parent)
    (cond ((null parent) 1)
          ((islistik parent) (print parent))
          (T (and (print parent) (print (children parent)) (print_tree (car (children parent))) (print_tree (cadr (children parent)))))
    )
)

(defun case1 (parent)
    (let* ((son1        (get_left_child parent))
           (uncle_son2    (get_right_child parent))
           (grandson  (get_left_child (get_left_child parent))))
            (print "-------")
            (print son1)
            (print grandson)
            (print uncle_son2)
            (print "-------")
          (and (equal "red" (color son1))
               (equal "red" (color uncle_son2))
               (equal "red" (color grandson)))
    )
)

(defun case2_left (parent)
    (let* ((uncle_son2         (get_right_child parent))
           (grandson    (cadr (children (get_left_child parent))))
           (son1    (get_left_child parent)))
            
            (print "-------")
            (print son1)
            (print grandson)
            (print uncle_son2)
            (print "-------")

          (and (equal "black" (color uncle_son2))
               (equal "red"   (color son1))
               (equal "red"   (color grandson)))
    )
)

(defun case2_right (parent)
    (let* ((son1        (get_right_child parent))
           (grandson        (car (children (get_right_child parent))))
           (uncle_son2        (get_left_child parent)))
            
            (print "-------")
            (print son1)
            (print grandson)
            (print uncle_son2)
            (print "-------")

          (and (equal "black" (color uncle_son2))
               (equal "red"   (color son1))
               (equal "red"   (color grandson)))
    )
)



(defun case3_left (parent)
    (let* ((son1          (get_left_child parent))
           (uncle_son2    (get_right_child parent))
           (grandson      (car (children (get_left_child parent))))
           )

            (print "-------")
            (print son1)
            (print grandson)
            (print uncle_son2)
            (print "-------")

          (and (equal "red" (color son1))
               (equal "black" (color uncle_son2))
               (equal "red" (color grandson)))
    )
)

(defun case3_right (parent)
    (let* ((son1        (get_right_child parent))
           (uncle_son2  (get_left_child parent))
           (grandson    (cadr (children (get_right_child parent)))))

            (print "-------")
            (print son1)
            (print grandson)
            (print uncle_son2)
            (print "-------")

          (and (equal "red" (color son1))
               (equal "black" (color uncle_son2))
               (equal "red" (color grandson)))
    )
)


(defun case1_impl (parent)
    (cond ((is_root parent) (swap_color_red_up parent))
          (T (and (swap_color_red_up parent) (balance_tree nil (root parent))))
    )
)

(defun case2_impl_left (parent)
    (cond ((is_root parent)  (toblack (right_turn (small_left_turn parent))))
          (T (and (right_turn (small_left_turn parent)) (balance_tree (root parent)))))
)

(defun case2_impl_right (parent)
    (cond ((is_root parent) (toblack (left_turn (small_right_turn parent))))
          (T (and (left_turn (small_right_turn parent)) (balance_tree (root parent)))))
)

(defun case3_impl_left (parent)
    (cond ((is_root parent) (toblack (left_turn parent)))
          (T (and (left_turn parent) (balance_tree (root parent)))))
)

(defun case3_impl_right (parent)
    (cond ((is_root parent) (toblack (right_turn parent)))
          (T (and (right_turn parent) (balance_tree (root parent)))))
)

(defun balance_tree (parent)
    (cond 
            ; ((and (null root) (null parent)))
            ; ((and (null root) (is_root parent)) (and (print "0") (balance_tree parent parent)))
            ; ((case1 parent)                           (and (print "1") (swap_color_red_up parent) (balance_tree nil (root parent))))
            ; ((case2_left parent)                      (and (print "2") (left_turn (small_left_turn parent)) (balance_tree nil (root parent))))
            ; ((case2_right parent)                     (and (print "3") (right_turn (small_right_turn parent)) (balance_tree nil (root parent))))
            ; ((case3_left parent)                      (and (print "4") (left_turn parent) (balance_tree nil (root parent))))
            ; ((case3_right parent)                     (and (print "5") (right_turn parent) (balance_tree nil (root parent))))
            ; ((not (null root)) parent)
            ((case1 parent)                           (and (print "1") (case1_impl parent)))
            ((case2_left parent)                      (and (print "2") (case2_impl_left parent)))
            ((case2_right parent)                     (and (print "3") (case2_impl_left parent)))
            ((case3_left parent)                      (and (print "4") (case3_impl_right parent)))
            ((case3_right parent)                     (and (print "5") (case3_impl_right parent)))
            (t                                        (and (print "6") parent))
    )
)

(defun depth_trav (parent)
    (cond 
            ((null parent) 1)
            ((islistik parent) (balance_tree (root (root parent))))
            (T (depth_trav (and (balance_tree (car (children parent))) (balance_tree (cadr (children parent))))))
    )
)

(defun merge_tree (src dst)
    (cond ((null src) 1)
          ((islistik src) (and (insert dst (tored src)) (balance_tree dst)))
          (T (and   (and (insert dst (tored src)) 
                         (balance_tree dst))
                  (merge_tree (car (children src)) dst) 
                  (merge_tree (cadr (children src)) dst)))
    )
)

(setf n0 (make-instance 'node))
(setf n1 (make-instance 'node))
(setf n2 (make-instance 'node))
(setf n3 (make-instance 'node))
(setf n4 (make-instance 'node))
(setf n5 (make-instance 'node))
(setf n6 (make-instance 'node))


(setf (val n3) 5)
(setf (color n3) "black")
(setf (children n3) (list nil nil))
(setf (root n3) n1)

(setf (val n5) 11)
(setf (color n5) "black")
(setf (children n5) (list nil nil))
(setf (root n5) n4)

(setf (val n6) 12)
(setf (color n6) "black")
(setf (children n6) (list nil nil))
(setf (root n6) n4)

(setf (val n4) 10)
(setf (color n4) "red")
(setf (children n4) (list n5 n6))
(setf (root n4) n0)

(setf (val n2) 3)
(setf (color n2) "black")
(setf (children n2) (list nil nil))
(setf (root n2) n1)

(setf (val n1) 4)
(setf (color n1) "red")
(setf (children n1) (list n2 n3))
(setf (root n1) n0)

(setf (val n0) 6)
(setf (color n0) "black")
(setf (children n0) (list n1 n4))
(setf (root n0) nil)

; (print_tree n0)
; (print_tree (depth_trav n3))
(setf n14 (make-instance 'node))
(setf (val n14) 14)
(setf (color n14) "black")
(setf (children n14) (list nil nil))

(setf n13 (make-instance 'node))
(setf (val n13) 13)
(setf (color n13) "black")
(setf (children n13) (list nil n14))


; (insert n0 n13)
; (print "---------")
; (print_tree n0)
; (print (root n13))
; (depth_trav n0)
; (print (root (root n3)))

(print (append '(1) (list)))
(merge_tree n13 n0)
(print_tree n0)


