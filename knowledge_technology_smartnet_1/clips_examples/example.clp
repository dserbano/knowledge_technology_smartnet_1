(defrule rule1 (a) => (assert (data)))
(defrule rule2 (data) => (printout t "Hello"))
(defrule rule3 (data) => (printout t "there"))
(defrule rule4  => (printout t "nnnn"))
