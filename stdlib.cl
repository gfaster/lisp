(defmacro setq (var val)
  `(set ',var ,val))

(defmacro setq2 (v1 v2 e)
  (list 'progn (list 'setq v1 e) (list 'setq v2 e)))

(defun funcall (fn &rest args)
  (apply fn args))

(defmacro bootstrap-let (vars defs &body body)
  `((lambda ,vars (progn ,@body)) ,@defs))


(defmacro for-each (it l &body body)
  `((bootstrap-let 
      (__label l) 
      ((gensym) ,l) 
      (tagbody 
	__label 
	(if l
	  (bootstrap-let 
	    (it)
	    (car l)
	    ,@body 
	    (setq l (cdr l))
	    (go __label) )
	  ())))))

(defmacro bootstrap-until-nil (pred &body body)
  `((lambda __label 
     (tagbody 
       __label 
       (if ,pred 
	 (progn ,@body) 
	 (go __label))))
     'sym))



(defun mapcar (l proc)
  (bootstrap-let 
    (head) 
    ((cons (funcall proc (car l)) nil))
    (bootstrap-let 
      (it)
      (head)
      (bootstrap-until-nil
	(setq l (cdr l))
	(setcdr it (cons (funcall proc (car l)) nil))
	(setq it (cdr it))))))




(defun cadr (l) (car (cdr l)))

;; (defun unzip (l)
;;   (bootstrap-let 
;;     (left right) 
;;     ((car l) (cadr l))
;;     ((bootstrap-until-nil
;;        )
;;      )))


;; (defmacro let (vars body)
  
