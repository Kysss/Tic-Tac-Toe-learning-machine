(load "lp.l")
(setf grid '(nw n ne w c e sw s se))
(setf x-member '(X1 X2 X3 X4 X5))
(setf o-member '(O1 O2 O3 O4))
(setf (symbol-plist 'directions)'(nw 0 n 1 ne 2 w 3 c 4 e 5 sw 6 s 7 se 8))
					;simulate the play of a game
(defmethod play (&aux play avail move)
  (setf play () )
  (setf avail '(nw n ne w c e sw s se))
  (dolist (player '(x o x o x o x o x))
    (cond
     ((eq player 'x)
      (setf move (select avail))
      (setf avail (remove move avail ))
      (setf play (snoc move play))
      )
     ((eq player 'o)
      (setf move (select avail))
      (setf avail (remove move avail))
      (setf play (snoc move play))
      )
     )
    )
  play
  )

(defun select (list)
  (nth (random (length list)) list)
  )
(defmethod visualize ( (play list ) &aux save saveplay p)

  (setf annotatedsymbol '(X1 O1 X2 O2 X3 O3 X4 O4 X5))

  (dolist (x grid)
    (setf p (position x play))
    (if (null p)
	(setf symbol '--)
      (setf symbol (nth p annotatedsymbol))
      )
 (if	(or (or (eq x 'ne)(eq x 'e))(eq x 'se))
     (format t "~A~%" symbol)
   (format t "~A " symbol)
    
    )

 )

  )



;;needs to fix this so that displays dashes
;(defmethod visualize (( play list)&aux L )
;  (setf L '(- - - - - - - - -))
;  (setf annotatedsymbol '(X1 O1 X2 O2 X3 O3 X4 O4 X5))
;  (dolist (x annotatedsymbol)
;;    (setf current (nth (position x annotatedsymbol) play))
  ;  (setf (nth (get 'directions current) L ) x)
;    )
 ; (dolist (y L)
 ;   
  ;  (if (or (or (eq (position y L) 2)(eq (position y L) 5))(eq (position y L) 8))
;	(format t "~A ~%" y)
   ;   (format t "~A " y)
   ;   )
  ;  )
 ;)

(defmethod visualize2 ((play list) &aux L)
  (setf xolist '(- - - - - - - - -))
  (setf annotatedsymbol '(X1 O1 X2 O2 X3 O3 X4 O4 X5))
 
  (dolist (x annotatedsymbol)
   
    (setf current (nth (position x annotatedsymbol) play))
    (setf pos (get 'directions current))
    (if (not (eq pos nil))
	(setf xolist (rep-elt xolist pos x))
      (setf xolist (rep-elt xolist pos '--)))

    )
   (dolist (y xolist)
    
     (if (or (or (eq (position y xolist) 2)(eq (position y xolist) 5))
	     (eq (position y xolist) 8))
	(format t "~A ~%" y)
      (format t "~A " y)
      )
    )
 

   )

;;putting it into a list
(defmethod transform (( play list)&aux L )
  (setf L '(- - - - - - - - -))
  (setf annotatedsymbol '(X1 O1 X2 O2 X3 O3 X4 O4 X5))
  (dolist (x annotatedsymbol)
    (setf current (nth (position x annotatedsymbol) play))
    (setf (nth (get 'directions current) L ) x)
    )
  L
  )

;(defmethod analyze ( ( play list )&aux save)
;  (setf status 'D)
 ; (setf annotatedsymbol '(X1 O1 X2 O2 X3 O3 X4 O4 X5))
 ; (dolist (x grid)
 ;   (setf p (position x play))
 ;   (if (null p)
;	(setf symbol '--)
 ;     (setf symbol (nth p annotatedsymbol))
 ;     )
 ;   (setf save (snoc symbol save))
 ;   (setf result (check play))
  ;  (cond
 ;    ((and (eq result t) (member symbol x-member))
;	 (setf status 'w))
 ;    ( (and (eq result t) (member symbol o-member))
;	(setf status 'l))
 ;   ( (and (eq result nil))
;	     (setf status 'D)))
 ;   )
 ; status
 ; )

;;//NEEDS REVISION DOESNT ANALYZE CORRECTLY

(defmethod analyze ((play list)&aux xolist annotatedsymbol current result)
  (setf result 'D)
 ; (format t "play = ~A~%" play)
  (setf xolist '(- - - - - - - - -))
  (setf annotatedsymbol '(X1 O1 X2 O2 X3 O3 X4 O4 X5))
 
  (dolist (x annotatedsymbol)
   
    (setf current (nth (position x annotatedsymbol) play))
    (setf pos (get 'directions current))
    (setf xolist (rep-elt xolist pos x))
  ;  (setf (nth (get 'directions current) xolist ) x)
 ;;  (setf xolist (cons '() xolist))
  ;  (format t "xolist = ~A~%" xolist)
   ;; (setf xolist dup)
 ;;   (print x)
    (if (checkwinstate xolist)
	(let ()
	  (if (member x x-member)
	      (setf result 'W)
	    (setf result 'L)
	    )
	  (return)
	  )
      )
    )
  ;(makunbound 'xolist)
  result
  )

(defun rep-elt (list &optional n new)
  (cond ((null n)
	 list)
	((> n 0)
	 (cons (car list)(rep-elt (cdr list)(1- n)new)))
	((eq n 0)
	 (cons new (cdr list))
	 )))


;;check whether there is a win
(defmethod checkwinstate(( s list) )
;;  (setf s (transform play))
  (or (checkpos s 0 3 6)
  (or (checkpos s 0 1 2)
  (or (checkpos s 0 4 8)
  (or (checkpos s 2 4 6)
  (or (checkpos s 3 4 5)
  (or (checkpos s 1 4 7)
  (or (checkpos s 2 5 8)
  (checkpos s 6 7 8))))))))
 ;; //
  )

(defmethod checkdrawstate((play list))
  (eq (checkwinstate play) nil)
  )

;;helper method to check whether the given three position have the same symbol--X  or  O
(defmethod checkpos ((list list) n1 n2 n3)
  (cond
   (
    (not (eq (and (and(member (nth n1 list) x-member)
	     (member (nth n2 list) x-member))
	 (member (nth n3 list) x-member)) nil))
    t)
   (
    (not (eq (and (and(member (nth n1 list) o-member)
	     (member (nth n2 list) o-member))
	 (member (nth n3 list) o-member)) nil))
    t)
    (t nil)
   )

  )


(defmethod demo (&aux p)
  (setf p (play))
  (format t "~A" p )
  (visualize p )
  (format t "~A~%" (analyze p ))
  nil
  )

(defmethod stats (( n number)(demo t ) &aux w l d p result)
  (if demo (format t "Begin gathering statistics ...~%"))
  ( setf w 0 l 0 d 0)
  (dotimes ( i n )
    (setf p (play))
    (if demo (format t "~A~%" p ))
    (if demo (visualize p ))
    (setf result (analyze p ))
    (if demo (format t "~A~%" result))
    (cond
     ((eq result 'w)(setf w (+ w 1 )))
     ((eq result 'l)(setf l (+ l 1 )))
     ((eq result 'd)(setf d (+ d 1 )))
     )
    )
  (setf results (mapcar #'probability (list w l d)(list n n n)))
  (if demo (format t "End gathering statistics~%"))
  (mapcar #'list '(w l d) results)
  )

(defmethod probability ((special integer)(total integer))
  (/ (float special)(float total))
  )

(defmethod matches ((first list)(second list))
  (cond
   ((null first)t)
   ((equal (car first)(car second))(matches (cdr first)(cdr second)))
   ( t nil)
   )
  )

(defclass player ()
  ((name :accessor player-name :initarg :name :initform 'emanon))
  )

(defclass random-machine-player (player)())

(defmethod display ( (p random-machine-player))
  (format t "RANDOM MACHINE PLAYER...~%")
  (format t "NAME = ~A~%" (player-name p))
  (format t "~%")
  nil
  )

(defclass human-player (player)())

(defmethod display (( p human-player))
  (format t "HUMAN PLAYER...~%")
  (format t "NAME = ~a~%" (player-name p))
  (format t "~%")
  nil
  )

(defmethod make-move ((p random-machine-player)(report t) &aux move)
  (if report (format t "BEGIN RANDOM PLAYER MOVE...~%"))
  (setf move (select *avail*))
  (if report (format t "randomly selecting ~A for my move~%" move))
  (setf *avail* (remove move *avail*))
  (if report (format t "END RANDOM PLAYER MOVE...~%"))
  move
  )

(defmethod make-move ((p human-player)(report t) &aux move)
  (if report (format t "BEGIN HUMAN PLAYER MOVE...~%"))
  (format t "PLEASE SELECT A MOVE FROM ~A~%" *avail*)
  (setf move (read))
	(cond
	 ((not (member move *avail*))
	  (make-move p)
	  )
	 (t
	  (setf *avail* (remove move *avail*))
	  move
	  )
	 )
	(if report (format t "END HUMAN PLAYER MOVE ~%"))
	move
	)

(defmethod generic-play ((x player)(o player)(report t) &aux move)
  (setf *avail* '(nw n ne w c e sw s se))
  (setf *play-so-far* ())
  (dolist (player '(x o x o x o x o x))
    (if (or report (equal (type-of o) 'human-player-machine))
	(visualize *play-so-far*)
      )
    (cond
     ((eq player 'x)
      (setf move (make-move x report))
      )
     ((eq player 'o)
      (setf move (make-move o report))
      )
     )
    (setf *play-so-far* (snoc move *play-so-far*))
    (if (game-over-p *play-so-far*)(return nil))
    )
  *play-so-far*
  )

(defmethod game-over-p ((play list))
  (cond
   ((line-p (odd play)) 'w)
   ((line-p (even play)) 'l)
   ((= (length play) 9) 'd)
   (t nil)
   )
  )

(defmethod odd ((l list))
  (cond
   ((null l )())
   ((null (cdr l))(list (car l)))
   (t (cons (car l )(odd (cddr l))))
   )
  )
(defmethod even((l list))
  (cond
   ((null l)())
   ((null (cdr l))())
   (t (cons (cadr l)(even (cddr l))))
   )
  )
(defmethod line-p ((l list))
  (cond
   ((<(length l)3)
    nil
    )
   ((=(length l)3)
    (line (first l)(second l)(third l))
    )
   ((=(length l)4)
    (or
     (line (first l)(second l)(third l))
     (line (first l)(second l)(fourth l))
     (line (first l)(third l)(fourth l))
     (line (second l)(third l)(fourth l))
     )
    )
   ((=(length l)5)
    (or
      (line (first l)(second l)(third l))
      (line (first l)(second l)(fourth l))
      (line (first l)(second l)(fifth l))
      (line (first l)(third l)(fourth l))
      (line (first l)(third l)(fifth l))
      (line (first l)(fourth l)(fifth l))
      (line (second l)(third l)(fourth l))
      (line (second l)(third l)(fifth l))
      (line (second l)(fourth l)(fifth l))
      (line (third l)(fourth l)(fifth l))
      )
    )
   )
  )

(defmethod line ( a b c)
  (and (eq a b)(eq a c)(eq b c))
  )

(defmethod demo-random-random (&aux p x o)
	(setf x (make-instance 'random-machine-player))
	(setf o (make-instance 'random-machine-player))
	(setf p (generic-play x o t))
	(format t "~A~%" p)
	(visualize p)
	(format t "~A~%" (analyze p))
	nil
)
(defmethod demo-random-human (&aux p x o)
  (setf x (make-instance 'random-machine-player))
  (setf o (make-instance 'human-player))
  (setf p (generic-play x o t))
  (format t "~A~%" p)
  (visualize p)
  (format t "~A~%" (analyze p))
  nil
  )
