;;; ==================================================================
;;; FLANKER-DEVICE.LISP
;;; ==================================================================
;;; (c) 2018, Andrea Stocco, University of Washington
;;;           stocco@uw.edu
;;; ==================================================================
;;; A class that provide an ACT-R GUI interface for a modified
;;; version of the Flanker Task.
;;; ==================================================================
;;;
;;; How to use this device
;;; ----------------------
;;;   1. Load this file first, before loading any model.
;;;   2. *After* loading this device, load any model. A simple
;;;      model performing visuo-motor operations is given in the
;;;      response-monkey.lisp file.
;;;   3. Before running the model call the "smart-reload" function:
;;;
;;;                    CL-USER> (smart-reload)
;;;
;;;      This will ensure that the device is properly installed
;;;      and connected to the model.
;;;   4. The device will produce single type of visual-location,
;;;      called "flanker-stimulus-location", which contains a
;;;      slot "phase" that indicates the task's phase (pause,
;;;      stimulus, done).
;;;   5. Every stimulus is represented by a 'flanker-stimulus'
;;;      chunk, whose kind is 'stimulus' and with special slots for
;;;      left, center, and right stimulus. E.g., the stimulus "><>"
;;;      is represented by chunk with slots:
;;;
;;;          ... 
;;;          left >
;;;          center <
;;;          right >
;;;
;;;   6. Note that the symbols ">" and "<" are represented by the
;;;      chunks "I>" and "I<" because they cannot be used to name
;;;      proper chunks in ACT-R.
;;;
;;; ------------------------------------------------------------------


;;; ------------------------------------------------------------------
;;; ACT-R Functions
;;; ------------------------------------------------------------------

(defun act-r-loaded? ()
  "Cheap hack to check whether ACTR is loaded"
  (member :act-r *features*))

;;; ------------------------------------------------------------------
;;; Modified action selection
;;; ------------------------------------------------------------------
;;; This is the code that modifies the action selection system in
;;; ACT-R, making it more consistent with the real basal ganglia.
;;; (see Stocco, "A biologically-plausible action selection system
;;; for cognitive architectures", submitted, Cog. Sci.)
;;; ------------------------------------------------------------------

(defparameter *d1* 1 "Dopamine D1 receptor density")

(defparameter *d2* 1 "Dopamine D2 receptor density")

(defparameter *bias* 10 "Processing bias")

(defparameter *error-penalty* -1)

(defun bg-reward-hook (production reward time)
  (declare (ignore time))
  (let* ((pname (symbol-name production))
	 (start (subseq pname 0 4)))

    (cond ((string-equal start "PROC")
	   (* *d1* reward))
	  ((string-equal start "DONT")
	   (* *d2* reward))
	  (t
	   0.0))))

;; ---------------------------------------------------------------- ;;
;; Some utilities
;; ---------------------------------------------------------------- ;;

(defun pick (lst)
  "Picks up an element from a list"
  (when  (listp lst)
    (elt lst (random (length lst)))))


(defun scramble (lst &optional (sofar nil))
  "Scrambles a list of different elements"
  (if (null lst)
      sofar
    (let ((picked (pick lst)))
      (scramble (remove picked lst) (cons picked sofar)))))

(defun scramble* (lst)
  "Scrambles any list of objects"
  (let ((l (length lst))
        (pos nil))
    (dotimes (i l)
      (push i pos))
    (mapcar #'(lambda (x) (elt lst x)) (scramble pos))))

xo
(defun mean (&rest nums)
  (when (every #'numberp nums)
    (/ (reduce #'+ nums)
       (length nums))))


(defun divide-into-pairs (lst &optional (partial nil) (open nil))
  "Recursively divides a list into pairs"
  (cond ((null lst)
	 (append partial open))
	((= (length (car open)) 2)
	 (divide-into-pairs (rest lst)
			    (append partial open)
			    (list (list (first lst)))))
	((= (length (car open)) 1)
	 (divide-into-pairs (rest lst)
			    partial
			    (list (list (caar open)
					(first lst)))))
	(t
	 (divide-into-pairs (rest lst)
			    partial
			    (list (list (first lst)))))))


;;; ------------------------------------------------------------------
;;; FLANKER TASK DATA STRUCTURES
;;; ------------------------------------------------------------------

(defparameter *flanker-rule* '((< . left) (> . right)))

(defparameter *responses* '((f . left) (j . right)))

(defparameter *flanker-stimuli* '(< >))

(defparameter *default-flanker-congruent-stimuli* '((> > >)
						    (< < <)))



(defparameter *default-flanker-incongruent-stimuli* '((> < >)
						      (< > <)))

(defun flanker-stimulus? (lst)
  (and (consp lst)
       (oddp (length lst))
       (every #'(lambda (x) (member x *flanker-stimuli*))
	      lst)))


(defun stimulus-correct-response (stimulus
				  &optional (rule *flanker-rule*))
  (when (flanker-stimulus? stimulus)
    (let* ((n (length stimulus))
	   (pos (floor (/ n 2)))
	   (focus (nth pos stimulus)))
      (cdr (assoc focus rule)))))
      

(defun stimulus-congruent? (stimulus)
  (and (flanker-stimulus? stimulus)
       (let ((testbed (first stimulus)))
	 (every #'(lambda (x) (equalp x testbed))
		stimulus))))

(defun stimulus-incongruent? (stimulus)
  (not (stimulus-congruent? stimulus)))

(defun stimulus-type (stimulus)
  (if (stimulus-congruent? stimulus)
    'congruent
    'incongruent))

(defun make-flanker-trial (stim)
  (let ((trial (list stim 0 0 (stimulus-correct-response stim) nil nil)))
    (set-trial-type trial (stimulus-type stim))
    trial))

(defun trial-stimulus (trial)
  (nth 0 trial))

(defun set-trial-stimulus (trial stimulus)
  (when (flanker-stimulus? stimulus)
    (setf (nth 0 trial) stimulus)))

(defun trial-onset-time (trial)
  (nth 1 trial))

(defun set-trial-onset-time (trial tme)
  (setf (nth 1 trial) tme))

(defun trial-response-time (trial)
  (nth 2 trial))

(defun set-trial-response-time (trial tme)
  (setf (nth 2 trial) tme))

(defun trial-correct-response (trial)
  (nth 3 trial))

(defun set-trial-correct-response (trial response)
  (setf (nth 3 trial) response))

(defun trial-actual-response (trial)
  (nth 4 trial))

(defun set-trial-actual-response (trial response)
  (setf (nth 4 trial) response))

(defun trial-type (trial)
  (nth 5 trial))

(defun set-trial-type (trial typ)
  (setf (nth 5 trial) typ))

(defun trial-congruent? (trial)
  (equalp (trial-type trial) 'congruent))

(defun generate-stimuli ()
  (let ((result nil))
    (dolist (stimulus *default-flanker-congruent-stimuli*)
      (dotimes (i 75)
	(push (copy-seq stimulus) result)))
    (dolist (stimulus *default-flanker-incongruent-stimuli*)
      (dotimes (i 25)
	(push (copy-seq stimulus) result)))
    result))

(defun generate-trials (stim-list)
  (mapcar #'make-flanker-trial stim-list))

(defun trial-rt (trial)
  (- (trial-response-time trial)
     (trial-onset-time trial)))

(defun trial-accuracy (trial)
  (if (equal (trial-correct-response trial)
	     (trial-actual-response trial))
      1
      0)) 

;;;  Simon Task object ------------------------------------------- ;;;

(defclass flanker-task ()
  ((phase :accessor task-phase
	  :initform nil)
   (index :accessor index
	  :initform nil)
   (trials :accessor trials
	   :initform (generate-trials (generate-stimuli)))
   (current-trial :accessor current-trial
		  :initform nil)
   (experiment-log :accessor experiment-log
		   :initform nil))
  (:documentation "A manager for the Flanker task"))

(defmethod init ((task flanker-task))
  "Initializes the Flanker task manager"
  (unless (null (trials task))
    (setf (index task) 0)
    (setf (experiment-log task) nil)
    (setf (trials task) (scramble* (trials task)))
    (setf (current-trial task)
	  (nth (index task) (trials task)))
    (setf (task-phase task) 'stimulus)))


(defmethod respond ((task flanker-task) key)
  "Records a response in the Flanker task"
  (unless (null (current-trial task))
    (let* ((trial (current-trial task))
	   (response (cdr (assoc key *responses*))))
      (set-trial-actual-response trial response)
      (when (act-r-loaded?)
	(set-trial-response-time (current-trial task)
				 (mp-time))
	;(if (= 1 (trial-accuracy (current-trial task)))
	;    (trigger-reward 1)
	;    (trigger-reward -1))
	(schedule-event-relative 0 #'next :params (list task))))))
            

(defmethod next ((task flanker-task))
  "Moves to the next step in a Flanker Task timeline"
  (cond ((equal (task-phase task) 'stimulus)
	 (setf (task-phase task) 'pause)
	 (push (current-trial task) (experiment-log task))
	 (setf (current-trial task) nil)
	 (when (act-r-loaded?)
	   (schedule-event-relative 1 'next :params (list task))))
	((equal (task-phase task) 'pause)
	 (incf (index task))
	 (cond ((>= (index task) (length (trials task)))
		(setf (task-phase task) 'done))
	       (t
		(setf (task-phase task) 'stimulus)
		(setf (current-trial task) (nth (index task)
						(trials task)))
		(when (act-r-loaded?)
		  (set-trial-onset-time (current-trial task)
					(mp-time)))))))
  (when (act-r-loaded?) 
    (schedule-event-relative 0 'proc-display :params nil)))


;;; ------------------------------------------------------------------
;;; ACT-R DEVICE INTERFACE
;;; ------------------------------------------------------------------
;;; These functions turn the Simon-Task class into an ACT-R device
;;; ------------------------------------------------------------------

(defmethod device-handle-keypress ((task flanker-task) key)
  "Converts the key into a symbol and passes it on to the task manager"
  (respond task (intern (string-capitalize (format nil "~a" key)))))

			   
(defmethod device-handle-click ((task flanker-task))
  "Does nothing"
  (declare (ignore task))
  nil)

(defmethod device-move-cursor-to ((task flanker-task) pos)
  "Does nothing"
  (declare (ignore task))
  nil)


(defmethod get-mouse-coordinates ((task flanker-task))
  "Does nothing"
  (declare (ignore task))
  (vector 0 0))

(defmethod cursor-to-vis-loc ((task flanker-task))
  "Does nothing"
  (declare (ignore task))
  nil)

(defparameter *chunk-symbols* '((> . i>)
				(< . i<))
  "ACT-R cannot use '<' and '>' as chunk names, so these must be adopted")

(defun translate-flanker-symbol (sym)
  (when (member sym *flanker-stimuli*)
    (cdr (assoc sym *chunk-symbols*)))) 

(defmethod build-vis-locs-for ((task flanker-task) vismod)
  (let ((results nil))
    (push  `(isa flanker-stimulus-location 
		 kind flanker-stimulus
		 value ,(task-phase task)
		 color black
		 screen-x 0
		 screen-y 0
		 height 400 
		 width 400)
	   results)
    (define-chunks-fct results)))
;  (if (equalp (task-phase task) 'stimulus)
;      (build-vis-locs-for (trial-stimulus (current-trial task));
;			  vismod)
;      (build-vis-locs-for (task-phase task);
;			  vismod)))

(defmethod build-vis-locs-for ((trial list) vismod)
  (let ((results nil))
    (push  `(isa flanker-stimulus-location 
		 kind flanker-stimulus
		 value stimulus
		 color black
		 screen-x 0
		 screen-y 0
		 height 400 
		 width 400)
	   results)
    (define-chunks-fct results)))

(defmethod build-vis-locs-for ((phase symbol) vismod)
  (let ((results nil))
    (push  `(isa flanker-stimulus-location 
		 kind screen
		 value ,phase
		 color black
		 screen-x 0
		 screen-y 0
		 height 400 
		 width 400)
	   results)
    (define-chunks-fct results)))


(defmethod vis-loc-to-obj ((task flanker-task) vis-loc)
  "Transforms a visual-loc into a visual object"
  (let ((new-chunk nil)
	(phase (task-phase task))
	(stimulus (trial-stimulus (current-trial task))))
    (if (equal phase 'stimulus)
	(setf new-chunk (vis-loc-to-obj stimulus vis-loc))
	(setf new-chunk (vis-loc-to-obj phase vis-loc)))
    (fill-default-vis-obj-slots new-chunk vis-loc)
    new-chunk))


(defmethod vis-loc-to-obj ((stimulus list) vis-loc)
  "Transforms a stimulus into a visual object"
  (first (define-chunks-fct 
	     `((isa flanker-stimulus
		    kind flanker-stimulus 
		    left ,(translate-flanker-symbol (first stimulus))
		    center ,(translate-flanker-symbol (second stimulus))
		    right ,(translate-flanker-symbol (third stimulus))
		    )))))

(defmethod vis-loc-to-obj ((phase symbol) vis-loc)
  "Transforms a stimulus into a visual object"
  (first (define-chunks-fct 
	     `((isa flanker-screen
		    kind flanker-screen 
		    value ,phase
		    )))))

;;; ------------------------------------------------------------------
;;; DATA COLLECTION AND STATS
;;; ------------------------------------------------------------------

(defun analyze-log (log)
  "Analyzes the log of a single run"
  (let* ((incong (remove-if #'trial-congruent? log))
	 (cong (remove-if-not #'trial-congruent? log))
	 (correct-incong (remove-if-not #'(lambda (x) (= (trial-accuracy x) 1))
					incong))
	 (correct-cong (remove-if-not #'(lambda (x) (= (trial-accuracy x) 1))
					cong)))
    
    (if (or (null correct-incong)
	    (null correct-cong))
	;; If we don't have enough trials, return NA
	;;'((:congruent :na) (:incongruent :na))
	'(:na :na :na :na)
	;; Otherwise, compute accuracies and RTs (on correct trials)
	(let* ((cong-acc (apply #'mean (mapcar #'trial-accuracy cong)))
	       (incong-acc (apply #'mean (mapcar #'trial-accuracy incong)))
	       (cong-rt (apply #'mean (mapcar #'trial-rt correct-cong)))
	       (incong-rt (apply #'mean (mapcar #'trial-rt correct-incong))))
	  (list cong-acc cong-rt incong-acc incong-rt)))))


(defun result? (lst)
  "A list is a result IFF it's made of at least four numbers"
  (and (>= (length lst) 4)
       (every #'(lambda (x) (or (numberp x)
				(keywordp x)))
	      lst)))

(defun result-congruent-accuracy (res)
  (nth 0 res))


(defun result-congruent-rt (res)
  (nth 1 res))


(defun result-incongruent-accuracy (res)
  (nth 2 res))


(defun result-incongruent-rt (res)
  (nth 3 res))

(defparameter *results* nil)
	  
(defun average-results (results)
  "Averages values across a list of results"
  (if (every #'result? results)
    (let* ((meanres nil)
	   (n (length (first results))))
      (dotimes (i n (reverse meanres))
	(let ((avg
	       (float
		(apply 'mean
		       (remove-if-not #'numberp
				      (mapcar #'(lambda (x) (nth i x))
					      results))))))
	  (push avg meanres))))
    (progn
      (format t "   Not every results is a result")
      (setf *results* results))))


;;; ------------------------------------------------------------------
;;; SMART RELOAD
;;; ------------------------------------------------------------------

(defun smart-reload ()
  (let ((flanker (make-instance 'flanker-task)))
    (init flanker)
    (reload)
    (install-device flanker)
    (proc-display)))
