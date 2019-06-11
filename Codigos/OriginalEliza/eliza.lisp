;
; Eliza chatbot
; see http://www.ulisp.com/show?240L
;

(defun match (pat in)
  (cond
   ((null pat) (null in))
   ((eq (car pat) '*) (wildcard pat in))
   ((eq (car pat) (car in)) (match (cdr pat) (cdr in)))
   (t nil)))

(defun wildcard (pat in)
  (cond
   ((match (cddr pat) in) (bind (cadr pat) nil) t)
   ((null in) nil)
   ((match pat (cdr in)) (bind (cadr pat) (car in)) t)
   (t nil)))

(defvar *viewpoint* '((I you) (you I) (me you) (am are) (was were) (my your)))

(defun swap (value)
  (let ((a (assoc value *viewpoint*)))
    (if a (cadr a) value)))

(defvar *bindings* nil)

(defun bind (var value)
  (cond
   ((assoc var *bindings*)
    (push (swap value) (cdr (assoc var *bindings*))))
   (t (push (cons var (swap value)) *bindings*))))

(defun subs (list)
  (cond
   ((null list) nil)
   (t
    (let ((a (assoc (car list) *bindings*)))
      (cond
       (a (append (cdr a) (subs (cdr list))))
       (t (cons (car list) (subs (cdr list)))))))))

(defvar *rules*
  '(((* x hello * y) (hello. what's up?))
    ((* x i want * y) (what would it mean if you got y ?) (why do you want y ?))
    ((* x i wish * y) (why would it be better if y ?))
    ((* x i hate * y) (what makes you hate y ?))
    ((* x if * y)
     (do you really think it is likely that y)
     (what do you think about y))
    ((* x no * y) (why not?))
    ((* x i was * y) (why do you say x you were y ?))
    ((* x i feel * y) (do you often feel y ?))
    ((* x i felt * y) (what other feelings do you have?))
    ((* x) (you say x ?) (tell me more.))))

(defun random-elt (list)
  (nth (random (length list)) list))

(defun eliza ()
  (loop
   (princ "> ")
   (let* ((line (read-line))
          (input (read-from-string (concatenate 'string "(" line ")"))))
     (when (string= line "bye") (return))
     (setq *bindings* nil)
     (print
      (dolist (r *rules*)
        (when (match (first r) input)
          (return 
           (subs (random-elt (cdr r)))))))
     (terpri))))
    