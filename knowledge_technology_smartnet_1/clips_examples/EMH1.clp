; This is a very simple example of a CLIPS knowledge base,
; just using the pattern matching to create new knowledge.

; To run this example:
; 1) start CLIPSWin from Explorer
; 2) load the KB with (load "emh1.txt")
; 3) initialize with (reset)
; 4) run the KB with (run)
; 5) to view the facts generated, enter (facts)
; The current fact is for a patient with symptoms of measles.
; To change this, edit the deffacts at the end of this file.

; Before we can define rules and facts, we have to define the
; "variables" used with deftemplate, which defines the concept
; name, as well as the names of all attributes which could be
; given values. 

; A "Patient" may have values for fever, spots, rash, sore_throat
; and innoculated.

(deftemplate Patient
   (slot fever)
   (slot spots)
   (slot rash)
   (slot sore_throat)
   (slot innoculated))

; Thes deftemplates represent conclusions which we may assign
; values to as a result of the inference.

(deftemplate Diagnosis
   (slot diagnosis))

(deftemplate Treatment
   (slot treatment))

; In its simplest form, a rule just has a right side which is a
; "template" that the inference engine will try to match to some
; fact. In this case, it matches a Patient with specific values 
; for fever, spots, and innoculated (the other values don't matter).

; The left side of the => are actions to take if the RHS matches a
; fact. In this case, we assert a new fact (Diagnosis (diagnosis measles))
; and printout the string "Measles diagnosed" to the terminal (t), 
; followed by a return (crlf).

(defrule Measles
   (Patient (fever high) (spots yes) (innoculated no))
   =>
   (assert (Diagnosis (diagnosis measles)))
   (printout t "Measles diagnosed" crlf))

; We can also combine template matches, using the standard connectives
; of and, or, not. Note that the syntax of CLIPS is prefix-oriented.

(defrule Allergy1
   (and (Patient (spots yes))
        (or (not (Patient (fever high)))
            (Patient (innoculated yes))))       
   =>
   (assert (Diagnosis (diagnosis allergy)))
   (printout t "Allergy diagnosed from spots and lack of measles" crlf))   

(defrule Allergy2
   (Patient (rash yes))
   =>
   (assert (Diagnosis (diagnosis allergy)))
   (printout t "Allergy diagnosed from rash" crlf))

(defrule Flu
   (Patient (sore_throat yes) (fever mild | high))
   =>
   (assert (Diagnosis (diagnosis flu)))
   (printout t "Flu diagnosed" crlf))

; Rules for recommedaing treatments on the basis of
; Diagnosis facts created.

(defrule Penicillin
   (Diagnosis (diagnosis measles))
   =>
   (assert (Treatment (treatment pennicillin)))
   (printout t "Penicillin prescribed" crlf))

(defrule Allergy_pills
   (Diagnosis (diagnosis allergy))
   =>
   (assert (Treatment (treatment allergy_shot)))
   (printout t "Allergy shot prescribed" crlf))

(defrule Bed_rest
   (Diagnosis (diagnosis flu))
   =>
   (assert (Treatment (treatment bed_rest)))
   (printout t "Bed rest prescribed" crlf))

; Facts are created with deffacts (the can also
; be directly asserted while in CLIPS). The list
; consists of a name, and a list of facts.

(deffacts Symptoms
   (Patient (fever low) 
            (spots yes) 
            (rash no) 
            (sore_throat no) 
            (innoculated no)))