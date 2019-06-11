;
; Eliza chatbot
; see http://www.ulisp.com/show?240L
;

(defun correspondencia (padrao entrada)
  (cond
   ((null padrao) (null entrada))
   ((eq (car padrao) '*) (coringa padrao entrada))
   ((eq (car padrao) (car entrada)) (correspondencia (cdr padrao) (cdr entrada)))
   (t nil)))

(defun coringa (padrao entrada)
  (cond
   ((correspondencia (cddr padrao) entrada) (vincular (cadr padrao) nil) t)
   ((null entrada) nil)
   ((correspondencia padrao (cdr entrada)) (vincular (cadr padrao) (car entrada)) t)
   (t nil)))

(defvar *viewpoint* '((I you) (you I) (me you) (am are) (was were) (my your)))

(defun trocar (value)
  (let ((a (assoc value *viewpoint*)))
    (if a (cadr a) value)))

(defvar *ligacoes* nil)

(defun vincular (var value)
  (cond
   ((assoc var *ligacoes*)
    (push (trocar value) (cdr (assoc var *ligacoes*))))
   (t (push (cons var (trocar value)) *ligacoes*))))

(defun substituir (list)
  (cond
   ((null list) nil)
   (t
    (let ((a (assoc (car list) *ligacoes*)))
      (cond
       (a (append (cdr a) (substituir (cdr list))))
       (t (cons (car list) (substituir (cdr list)))))))))

(defvar *regras*
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

(defun aleatorio (list)
  (nth (random (length list)) list))

(defun eliza ()
  (loop
   (princ "Eliza> ")
   (let* ((line (read-line))
          (input (read-from-string (concatenate 'string "(" line ")"))))
     (when (string= line "bye") (return))
     (setq *ligacoes* nil)
     (print
      (dolist (r *regras*)
        (when (correspondencia (first r) input)
          (return 
           (substituir (aleatorio (cdr r)))))))
(terpri))))