;;;======================================================
;;;     JOBEX: The JOB EXpert system.
;;;     This helps you select a job based on your attitudes
;;;
;;;     CLIPS Version 6.31 
;;;
;;;     To execute, merely load, reset and run.
;;;======================================================

;;****************
;;  DEFFUNCTIONS *
;;****************

(defglobal ?*x* = 0)

(deffunction next-questionnaire (?questionnaire ?allowed-values)
   (printout t ?questionnaire)
   (bind ?answer (read))
   (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
   (while (not (member ?answer ?allowed-values)) do
      (printout t ?questionnaire)
      (bind ?answer (read))
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
   ?answer)

;;*****************
;;  INIT 
;;*****************

(defrule start
  (declare (salience 10000))
  =>
  (set-fact-duplication TRUE))

(deftemplate a-fact
   (slot name)
   (slot cf (default 0)))

(defrule combine-certainties-1 (declare (salience 100)(auto-focus TRUE))
  ?fact1 <- (a-fact (name ?id) (cf ?cf1))
  ?fact2 <- (a-fact (name ?id) (cf ?cf2))
  (test (neq ?fact1 ?fact2))
  (test (> ?cf1 0))
  (test (> ?cf2 0))
  =>
  (retract ?fact1)
  (modify ?fact2 (cf (+ ?cf1 (* ?cf2 (- 1 ?cf1))))))


(defrule combine-certainties-2 (declare (salience 100)(auto-focus TRUE))
  ?fact1 <- (a-fact (name ?id) (cf ?cf1))
  ?fact2 <- (a-fact (name ?id) (cf ?cf2))
  (test (neq ?fact1 ?fact2))
  (test (< ?cf1 0))
  (test (< ?cf2 0))
  =>
  (retract ?fact1)
  (modify ?fact2 (cf (+ ?cf1 (* ?cf2 (+ 1 ?cf1))))))

(defrule combine-certainties-3 (declare (salience 100)(auto-focus TRUE))
  ?fact1 <- (a-fact (name ?id) (cf ?cf1))
  ?fact2 <- (a-fact (name ?id) (cf ?cf2))
  (test (neq ?fact1 ?fact2))
  (test (> ?cf1 0))
  (test (< ?cf2 0))
  =>
  (retract ?fact1)
  (modify ?fact2 (cf (/ (+ ?cf1 ?cf2) (- 1 (min (abs ?cf1) (abs ?cf2)))))))

;;*******************
;;  QUESTIONNAIRE 
;;*******************

(deftemplate questionnaire
   (slot a-fact (default ?NONE))
   (slot the-questionnaire (default ?NONE))
   (multislot valid-answers (default ?NONE))
   (slot already-asked (default FALSE)))

(defrule ask-a-questionnaire
   ?f <- (questionnaire (already-asked FALSE)
                        (the-questionnaire ?the-questionnaire)
                        (a-fact ?the-fact)
                        (valid-answers $?valid-answers))
   =>
   (modify ?f (already-asked TRUE))
   (assert (a-fact (name ?the-fact)
                   (cf (next-questionnaire ?the-questionnaire ?valid-answers))))
   (bind ?*x* (+ ?*x* 1)))

(deffacts questionnaire-facts
  (questionnaire (a-fact f1)
                 (the-questionnaire "I spend most of my spare time actively socialising with friends, attending parties, shopping, etc. ")
                 (valid-answers -1 -0.5 0 0.5 1))
  (questionnaire (a-fact f2)
                 (the-questionnaire "I enjoy watching and playing team sports ")
                 (valid-answers -1 -0.5 0 0.5 1))
  (questionnaire (a-fact f3)
                 (the-questionnaire "Being around a group of people gives me energy ")
                 (valid-answers -1 -0.5 0 0.5 1))
  (questionnaire (a-fact f4)
                 (the-questionnaire "I would rather cook than go eat out in a restaurant. ")
                 (valid-answers -1 -0.5 0 0.5 1))
  (questionnaire (a-fact good-with-computers)
                 (the-questionnaire "I am very good with computers. ")
                 (valid-answers -1 -0.5 0 0.5 1))
  (questionnaire (a-fact f6)
                 (the-questionnaire "I improvise based on my past experience rather than looking for theoretical understanding ")
                 (valid-answers -1 -0.5 0 0.5 1))
  (questionnaire (a-fact f7)
                 (the-questionnaire "I am comfortable working with ambiguous or incomplete information/data and guessing its meaning.")
                 (valid-answers -1 -0.5 0 0.5 1))
  (questionnaire (a-fact f8)
                 (the-questionnaire "I like to work around targets, deadlines and routines for managing my life. ")
                 (valid-answers -1 -0.5 0 0.5 1)))

;;******************
;;  THE RULES 
;;******************

(defrule rule1
  (or
    (a-fact (name f1) (cf ?cf1))
    (a-fact (name likes-sports) (cf ?cf1)))
  (test (>= ?cf1 0.5))
  =>
  (assert (a-fact (name is-social) (cf (* 0.8 ?cf1))))))
  (assert (a-fact (name is-active) (cf (* 0.6 ?cf1))))))
  (assert (a-fact (name likes-outdoor-activities) (cf (* 0.8 ?cf1))))))
  (assert (a-fact (name likes-spending) (cf (* 0.8 ?cf1))))))

(defrule rule2
  (or
    (a-fact (name f1) (cf ?cf1))
    (a-fact (name is-follower) (cf ?cf1)))
  (test (<= ?cf1 -0.5))
  =>
  (assert (a-fact (name is-introvert) (cf (* 0.6 ?cf1))))))
  (assert (a-fact (name likes-indoor-activities) (cf (* 0.9 ?cf1))))))

(defrule rule3
  (and 
      (a-fact (name f2) (cf ?cf1))
      (a-fact (name f3) (cf ?cf2)))
  (test (>= ?cf1 0.5))
  (test (>= ?cf2 0.5))
  =>
  (assert (a-fact (name likes-sports) (cf (* 0.8 (max ?cf1 ?cf2)))))
  (assert (a-fact (name is-social) (cf (* 0.6 (max ?cf1 ?cf2)))))
  (assert (a-fact (name is-leader) (cf (* 0.7 (max ?cf1 ?cf2)))))
  (assert (a-fact (name likes-teaching) (cf (* 0.7 (max ?cf1 ?cf2))))))

(defrule rule4
  (and 
      (a-fact (name f2) (cf ?cf1))
      (a-fact (name f3) (cf ?cf2)))
  (test (<= ?cf1 -0.5))
  (test (<= ?cf2 -0.5))
  =>
  (assert (a-fact (name is-introvert) (cf (* 0.6 (max ?cf1 ?cf2)))))
  (assert (a-fact (name is-follower) (cf (* 0.7 (max ?cf1 ?cf2))))))

(defrule rule5
  (or 
      (a-fact (name f4) (cf ?cf1))
      (a-fact (name is-social) (cf ?cf1)))
  (test (>= ?cf1 0.5))
  =>
  (assert (a-fact (name is-introvert) (cf (* 0.9 ?cf1))))
  (assert (a-fact (name is-follower) (cf (* 0.7 ?cf1)))))

(defrule rule6
  (a-fact (name good-with-computers) (cf ?cf1))
  (test (>= ?cf1 0.5))
  =>
  (assert (a-fact (name likes-indoor-activities) (cf (* 0.9 ?cf1))))
  (assert (a-fact (name is-follower) (cf (* 0.7 ?cf1)))))

(defrule rule7
  (a-fact (name f6) (cf ?cf1))
  (test (<= ?cf1 -0.5))
  =>
  (assert (a-fact (name likes-studying) (cf (* 0.9 ?cf1))))
  (assert (a-fact (name likes-teaching) (cf (* 0.7 ?cf1)))))

(defrule rule8
  (a-fact (name f7) (cf ?cf1))
  (test (>= ?cf1 0.5))
  =>
  (assert (a-fact (name likes-studying) (cf (* 0.9 ?cf1))))
  (assert (a-fact (name likes-teaching) (cf (* 0.7 ?cf1)))))

(defrule rule9
  (a-fact (name f8) (cf ?cf1))
  (test (>= ?cf1 0.5))
  =>
  (assert (a-fact (name is-follower) (cf (* 0.9 ?cf1)))))


(defrule rule10
  (a-fact (name f8) (cf ?cf1))
  (test (<= ?cf1 -0.5))
  =>
  (assert (a-fact (name is-leader) (cf (* 0.8 ?cf1)))))


;;************************
;;* JOB SELECTION RULES
;;************************


(defrule jobsel1
  (test (>= ?*x* 7))
  (a-fact (name is-leader) (cf ?cf1))
  (test (>= ?cf1 0.4))
  (test (>= ?cf1 0.4))
  =>
  (printout  t "MANAGER" "0.6" crlf))

(defrule jobsel2
  (test (>= ?*x* 7))
  (and
    (a-fact (name is-leader) (cf ?cf1))
    (a-fact (name likes-studying) (cf ?cf2))
    (a-fact (name likes-teaching) (cf ?cf3)))
  (test (>= ?cf1 0.4))
  =>
  (printout  t "PROFESSOR" "0.6" crlf))

(defrule jobsel3
  (test (>= ?*x* 7))
  (and
    (a-fact (name is-leader) (cf ?cf1))
    (a-fact (name likes-studying) (cf ?cf2))
    (a-fact (name likes-teaching) (cf ?cf3)))
  (test (>= ?cf1 0.4))
  =>
  (printout  t "HR MANAGER" "0.6" crlf))

(defrule jobsel4
  (test (>= ?*x* 7))

  (and
    (a-fact (name is-leader) (cf ?cf1))
    (a-fact (name likes-studying) (cf ?cf2))
    (a-fact (name likes-teaching) (cf ?cf3)))
  (test (>= ?cf1 0.4))
  =>
  (printout  t "COMPUTER PROGRAMMER" "0.6" crlf))

(defrule jobsel5
  (test (>= ?*x* 7))

  (and
    (a-fact (name is-leader) (cf ?cf1))
    (a-fact (name likes-studying) (cf ?cf2))
    (a-fact (name likes-teaching) (cf ?cf3)))
  (test (>= ?cf1 0.4))
  =>
  (printout  t "MATHEMATICIAN" "0.6" crlf))

(defrule jobsel6
  (test (>= ?*x* 7))

  (and
    (a-fact (name is-leader) (cf ?cf1))
    (a-fact (name likes-studying) (cf ?cf2))
    (a-fact (name likes-teaching) (cf ?cf3)))
  (test (>= ?cf1 0.4))
  =>
  (printout  t "ARCHITECT" "0.6" crlf))

(defrule jobsel7
  (test (>= ?*x* 7))

  (and
    (a-fact (name is-leader) (cf ?cf1))
    (a-fact (name likes-studying) (cf ?cf2))
    (a-fact (name likes-teaching) (cf ?cf3)))
  (test (>= ?cf1 0.4))
  =>
  (printout  t "CONSTRUCTION WORKER" "0.6" crlf))

