(load "forest.lsp")

(fiveam:test tored-test
  (setf a (make-instance 'node))
  (tored a)
  (fiveam:is (equalp "r" (color a))))

(fiveam:test make-test
  (setf a (make-instance 'node))
  (fiveam:is (and 
                (equalp "b" (color a))
                (equalp -1000 (val a))
                (equalp (cons nil nil) (children a))
                (null (root a)))))

(fiveam:test tored-test
  (setf a (make-instance 'node))
  (tored a)
  (fiveam:is (equalp "r" (color a))))

(fiveam:test toblack-test
  (setf a (make-instance 'node))
  (toblack a)
  (fiveam:is (equalp "b" (color a))))

(fiveam:test get-left-child-normal
  (setf b (create_node 20 "b"))
  (setf c (create_node 30 "b"))
  (setf a (create_node 10 "r" nil (list b c)))
  (setf (root b) a)
  (setf (root c) a)
  (fiveam:is (equalp b (get_left_child a))))

(fiveam:test get-left-child-list
  (setf b (create_list))
  (setf c (create_node 30 "b"))
  (setf a (create_node 10 "r" nil (list b c)))
  (setf (root b) a)
  (setf (root c) a)
  (fiveam:is (equalp b (get_left_child a))))  

(fiveam:test get-left-child-list
  (setf b (create_list))
  (setf c (create_node 30 "b"))
  (setf a (create_node 10 "r" nil (list b c)))
  (setf (root b) a)
  (setf (root c) a)
  (fiveam:is (equalp b (get_left_child a))))  

(fiveam:test tolistik-test
  (setf c (create_node 30 "b"))
  (tolistik c)
  (islistik c)
  (fiveam:is (equalp -1000 (val (get_left_child c)))))

(fiveam:test islistik-test
  (setf c (create_list))
  (fiveam:is (equalp -1000 (val c))))

 (fiveam:test set-left-child-normal
  (setf c (create_node 30 "b"))
  (setf a (create_node 10 "r"))
  (set_left_child a c)
  (fiveam:is (and 
                (equalp a (root c))
                (equalp c (get_left_child a))
                (islistik c)
             )))   

 (fiveam:test set-left-child-notlistik
  (setf b (create_node 20 "b"))
  (setf c (create_node 30 "b"))
  (setf a (create_node 10 "r"))
  (set_left_child a c)
  (set_left_child a b)
  (fiveam:is (and 
                (equalp a (root c))
                (equalp c (get_left_child a))
                (islistik c)
             ))) 

 (fiveam:test set-left-child-nil
  (setf a (create_node 10 "r"))
  (fiveam:is (equalp a (set_left_child a nil)))) 

 (fiveam:test set-right-child-normal
  (setf c (create_node 30 "b"))
  (setf a (create_node 10 "r"))
  (set_right_child a c)
  (fiveam:is (and 
                (equalp a (root c))
                (equalp c (get_right_child a))
                (islistik c)
             )))  

 (fiveam:test set-right-child-notlistik
  (setf b (create_node 20 "b"))
  (setf c (create_node 30 "b"))
  (setf a (create_node 10 "r"))
  (set_right_child a c)
  (set_right_child a b)
  (fiveam:is (and 
                (equalp a (root c))
                (equalp c (get_right_child a))
                (islistik c)
             ))) 

 (fiveam:test set-right-child-nil
  (setf a (create_node 10 "r"))
  (fiveam:is (equalp a (set_right_child a nil)))) 

 (fiveam:test case0-not-test
  (setf a (create_node 10 "b"))
  (fiveam:is (null (case0 a))))

 (fiveam:test case0-yes-test
  (setf a (create_node 10 "r"))
  (fiveam:is (not (null (case0 a)))))

 (fiveam:test get-left-grandson-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_left_child a c)
  (fiveam:is (equalp c (get_left_grandson r))))

 (fiveam:test get-left-grandson-list-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (fiveam:is (isnullnode (get_left_grandson r))))

 (fiveam:test get-left-grandson-grand-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (fiveam:is (null (get_left_grandson a))))

 (fiveam:test get-right-grandson-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_right_child b c)
  (fiveam:is (equalp c (get_right_grandson r))))

 (fiveam:test get-right-grandson-list-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (fiveam:is (isnullnode (get_right_grandson r))))

 (fiveam:test get-right-grandson-grand-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (fiveam:is (null (get_right_grandson a))))

 (fiveam:test case1-yes-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_left_child a c)
  (fiveam:is (not (null (case1 r)))))

 (fiveam:test case1-list-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_left_child a c)
  (fiveam:is (null (case1 c))))

 (fiveam:test case1-parent-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_left_child a c)
  (fiveam:is (null (case1 a))))


 (fiveam:test case1-right-yes-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_right_child b c)
  (fiveam:is (not (null (case1_right r)))))

 (fiveam:test case1-right-list-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_right_child b c)
  (fiveam:is (null (case1_right c))))

 (fiveam:test case1-right-parent-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_right_child b c)
  (fiveam:is (null (case1_right a))))

 (fiveam:test get_lr_grandson_normal
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_right_child a c)
  (fiveam:is (equalp c (get_lr_grandson r))))

 (fiveam:test get_rl_grandson_normal
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_left_child b c)
  (fiveam:is (equalp c (get_rl_grandson r))))

 (fiveam:test case2-left-yes-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "b"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_right_child a c)
  (fiveam:is (not (null (case2_left r)))))

 (fiveam:test case2-left-list-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_right_child a c)
  (fiveam:is (null (case2_left a))))

 (fiveam:test case2-left-parent-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_right_child a c)
  (fiveam:is (null (case2_left a))))

 (fiveam:test case2-right-yes-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "b"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_left_child b c)
  (fiveam:is (not (null (case2_right r)))))

 (fiveam:test case2-right-list-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "b"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_left_child b c)
  (fiveam:is (null (case2_right a))))

 (fiveam:test case2-right-parent-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "b"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_left_child b c)
  (fiveam:is (null (case2_right a))))

 (fiveam:test case3-left-yes-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "b"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_left_child a c)
  (fiveam:is (not (null (case3_left r)))))

 (fiveam:test case3-left-list-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_left_child a c)
  (fiveam:is (null (case3_left a))))

 (fiveam:test case3-left-parent-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "r"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_left_child a c)
  (fiveam:is (null (case3_left a))))

 (fiveam:test case3-right-yes-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "b"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_right_child b c)
  (fiveam:is (not (null (case3_right r)))))

 (fiveam:test case3-right-list-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "b"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_right_child b c)
  (fiveam:is (null (case3_right a))))

 (fiveam:test case3-right-parent-test
  (setf r (create_node 10 "b"))
  (setf a (create_node 0 "b"))
  (setf b (create_node 20 "r"))
  (setf c (create_node 30 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_right_child b c)
  (fiveam:is (null (case3_right a))))

 (fiveam:test left-turn-normal-test
  (setf r (create_node 20 "b"))
  (setf a (create_node 10 "b"))
  (setf b (create_node 40 "b"))
  (setf c (create_node 30 "b"))
  (setf d (create_node 50 "b"))
  (set_left_child r a)
  (set_right_child r b)
  (set_left_child b c)
  (set_right_child b d)
  (fiveam:is (equalp b (toroot (left_turn r)))))

 (fiveam:test left-turn-normal-parent-and-son-test
  (setf r (create_node 20 "b"))
  (setf a (create_node 10 "b"))
  (setf b (create_node 40 "b"))
  (setf c (create_node 30 "b"))
  (setf d (create_node 50 "b"))
  (set_left_child r a)
  (set_right_child r b)
  (set_left_child b c)
  (set_right_child b d)
  (fiveam:is (equalp r (toroot (left_turn b)))))

 (fiveam:test right-turn-normal-test
  (setf r (create_node 20 "b"))
  (setf a (create_node 10 "b"))
  (setf b (create_node 40 "b"))
  (setf c (create_node 5 "b"))
  (setf d (create_node 15 "b"))
  (set_left_child r a)
  (set_right_child r b)
  (set_left_child a c)
  (set_right_child a d)
  (fiveam:is (equalp a (toroot (right_turn r)))))

 (fiveam:test right-turn-normal-parent-and-son-test
  (setf r (create_node 20 "b"))
  (setf a (create_node 10 "b"))
  (setf b (create_node 40 "b"))
  (setf c (create_node 5 "b"))
  (setf d (create_node 15 "b"))
  (set_left_child r a)
  (set_right_child r b)
  (set_left_child a c)
  (set_right_child a d)
  (fiveam:is (equalp r (toroot (right_turn a)))))

 (fiveam:test rl-turn-normal-test
  (setf r (create_node 17 "b"))
  (setf a (create_node 9 "b"))
  (setf b (create_node 19 "b"))
  (setf c (create_node 24 "b"))
  (setf d (create_node 75 "b"))
  (set_left_child r a)
  (set_right_child r b)
  (set_right_child b d)
  (set_left_child d c)
  (right_turn d)
  (left_turn b)
  (fiveam:is (equalp r (toroot a)))
 )

 (fiveam:test lr-turn-normal-test
  (setf r (create_node 17 "b"))
  (setf a (create_node 13 "b"))
  (setf b (create_node 19 "b"))
  (setf c (create_node 10 "b"))
  (setf d (create_node 12 "b"))
  (set_left_child r a)
  (set_right_child r b)
  (set_left_child a c)
  (set_right_child c d)
  (left_turn c)
  (right_turn a)
  (fiveam:is (equalp r (toroot a)))
 )

 (fiveam:test case2-impl-right-normal-test
  (setf r (create_node 17 "b"))
  (setf a (create_node 9 "b"))
  (setf b (create_node 19 "b"))
  (setf c (create_node 24 "r"))
  (setf d (create_node 75 "r"))
  (set_left_child r a)
  (set_right_child r b)
  (set_right_child b d)
  (set_left_child d c)
  (case2_impl_right b)
  (print_tree (toroot b))
  (fiveam:is (equalp r (toroot a)))
 )

(fiveam:run!)