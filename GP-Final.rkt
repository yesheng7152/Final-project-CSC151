#lang racket
(require csc151)
(require rackunit)

;;; Authors:
;;;   Yesheng Chen
;;;   Stefan Ilic
;;;   Stella Lee
;;;   Liam Liden
;;; Contents:
;;;   Code, Documentation, User Manual, and Conclusions for
;;;   our group project for CSC151.03
;;; Citations:
;;;   *The design for the perceptron learning algorithm was
;;;    inspired by 3Blue1Brown's 4 part video series on the subject
;;;    URL's: https://www.youtube.com/watch?v=aircAruvnKk
;;;           https://www.youtube.com/watch?v=IHZwWFHWa-w
;;;           https://www.youtube.com/watch?v=Ilg3gGewQ5U
;;;           https://www.youtube.com/watch?v=tIeHLnjs5U8
;;;   *The data set came from https://www.kaggle.com/uciml/breast-cancer-wisconsin-data

; +-------------+--------------------------------------------------------
; | User Manual |
; +-------------+

;;; Download data file from: https://www.kaggle.com/uciml/breast-cancer-wisconsin-data
;;; 1. Change the directory of clean-data-set to the directory of this newly downloaded data set
;;; 2. Input: (train weight-vector final-data-set)
;;;    * This will output the updated weight vector
;;; 3. Input: (map (section diagnoser-binary weight-vector <>) final-data-set)
;;;    * This will run the data-set through the algorithm to create "guesses".
;;;    * These guesses indicate whether the algorithm believes a tumor to be malignant or benign
;;;      1 indicates malignant, 0 indicates benign
;;;    * You can use (tally-all (map (section diagnoser-binary weight-vector <>) final-data-set))
;;;      to see how many guesses of malignant and benign were made total
;;; 4. Input: (accuracy final-data-set)
;;;    * This will output the overall accuracy of guesses compared to the actual diagnostics
;;; 5. Additional notes:
;;;    * Train can run multiple times, and the procedure 'train-multiple' exists for this purpose
;;;      Input: (train-multiple 'number-of-trainings-desired weight-vector final-data-set)
;;;    * Once trained on the initial data set, the weight vector can be used to predict on other sets
;;;      of data as well
;;;    * Other data sets must follow entry form
;;;    * weight-vector, times trained, and data set can all be changed to see different results
;;;    * The most optimal original weight-vector is already implemented
; +----------------+-----------------------------------------------------
; | Data Wrangling |
; +----------------+
;;; Goal:
;;; Make:
;;;   1) 'actual-diagnoses':
;;;      a table of the independent variables in row-format that is scaled
;;;      such that every data point is between -1 and 1 (inclusive)
;;;   2) 'scaled-indep-vars':
;;;      a list of the actual diagnoses (represented as 1's and 0's)
;;;   3) 'final-data-set':
;;;      a table that combines steps 1 and 2 above and adds a bias of 1 to each row

;;; Procedure:
;;;   cleaner
;;; Parameters:
;;;   file, a csv file path
;;; Purpose:
;;;   To create a table of data entries without category names, id number,
;;;   and standard error data.
;;; Produces:
;;;   result, a nested list
;;; Preconditions:
;;;   File contains Breast Cancer Wisconsin (Diagnostic) Data Set from
;;;   www.kaggle.com/uciml/breast-cancer-wisconsin-data/data
;;; Postconditions:
;;;   * (length result) = (decrement (length (read-csv-file file)))
;;;   * For every element in result, k, (length k)=22
;;;   * Elements of all entries with index of 0 and 12 to 22 of the original file
;;;     are not present in result
;;;   * (car (read-csv-file file)) is not present in result
(define cleaner
  (lambda (file)
    (let ([data (cdr (read-csv-file file))])
      (letrec ([cleaner-helper
                (lambda (entry pos)
                  (cond [(null? entry)
                         null]
                        [(= pos 1)
                         (if (equal? (car entry)
                                     "M")
                             (cons 1 (cleaner-helper (cdr entry)
                                                     (increment pos)))
                             (cons 0 (cleaner-helper (cdr entry)
                                                     (increment pos))))]
                        [(and (= pos 31)
                              (string? (car entry)))
                         (cons (string->number
                                (substring (car entry)
                                           0
                                           (decrement (string-length (car entry)))))
                               (cleaner-helper (cdr entry)
                                               (increment pos)))]
                        [(or (<= 2 pos 11)
                             (>= pos 22))
                         (cons (car entry)
                               (cleaner-helper (cdr entry)
                                               (increment pos)))]
                        [else
                         (cleaner-helper (cdr entry)
                                         (increment pos))]))])
        (map (section cleaner-helper <> 0) data)))))

;;; Make sure to change the file path
(define clean-data-set
  (cleaner "/home/leeseoye/Documents/BreastCancerData.csv"))

;;; Making a list of the actual diagnoses, represented as ones and zeros
;(1: Malignant 0: Benign)
(define actual-diagnoses
  (map car clean-data-set))

;;; Making a table of scaled independent variable in row-format

;;; Procedure:
;;;   just-the-indep-vars
;;; Parameters:
;;;   data-set, a nested list
;;; Purpose:
;;;   To create a table that excludes the first column of data-set
;;; Produces:
;;;   result, a nested list
;;; Preconditions:
;;;   data-set is a table that contains more than one column
;;; Postconditions:
;;;   * Result has same elements and orders except for the first column
;;;   * Number of elements in one row of result is one less than number of elements in one row of data-set
;;;     ex) (length (car result)) = (- (length (car data-set)) 1)
(define just-the-indep-vars
  (lambda (data-set)
    (map cdr data-set)))
  
;;; Procedure:
;;;   scale-one-column
;;; Parameters:
;;;   column-lst, list
;;; Purpose:
;;;   To scale one column so that every element is between -1 and 1 (inclusive)
;;; Produces:
;;;   result, list
;;; Preconditions:
;;;   every elements in the column-lst have to be real number
;;; Postconditions:
;;;   * (length column-lst) = (length result)
;;;   * When 0 <= n < (length result), 
;;;        (max = maximum value in the column-lst),
;;;        (min= minimum value in the column-lst)
;;;   * (list-ref result n) = (- (/ (list-ref column-lst n) (- max min)) 1)
(define scale-one-column
  (lambda (column-lst)
    (let* ([max-val
            (apply max column-lst)]
           [min-val
            (apply min column-lst)]
           [range-val
            (- max-val min-val)])
      (let kernel ([remaining column-lst])
        (if (null? remaining)
            null
            (cons (decrement (/ (car remaining)
                                range-val))
                  (kernel (cdr remaining))))))))

;;; Procedure:
;;;   scale-multiple-columns
;;; Parameters:
;;;   data-set, a nested list
;;; Purpose:
;;;   To scale every columns in data-set so that every element is between -1 and 1 (inclusive)
;;; Produces:
;;;   result, a nested list
;;; Preconditions:
;;;   (length data-set) = 20
;;;   every element of lists in the data-set is real number
;;; Postconditions:
;;;   * For every column in the result,
;;;     length of one column of data-set = length of one column of result
;;;   * when 0 <= n < (length one column of result), 
;;;         (max = maximum value in the column of data-set),
;;;         (min= minimum value in the column of data-set)
;;;   * (list-ref (the column result) n) = (- (/ (list-ref (the column data-set) n) (- max min)) 1)
(define scale-multiple-columns
  (lambda (data-set)
    (let kernel ([pos 0])
      (if (= pos 20)
          null
          (let ([column-lst
                 (map (section list-ref <> pos)
                      data-set)])
            (cons (scale-one-column column-lst)
                  (kernel (increment pos))))))))

;;; Procedure:
;;;   reformat-to-rows
;;; Parameters:
;;;   columned-data-set, a nested list
;;; Purpose:
;;;   To reformat a columned data set (each element represents a column)
;;;   into a rowed data set (each element represents a row)
;;; Produces:
;;;   result, a nested list
;;; Preconditions:
;;;   length of every element in columned-data-set is same 
;;; Postconditions:
;;;   * (length result) = (length (car columned-data-set))
;;;   * length of every element of result is (length columned-data-set)
;;;   * For every element
;;;      when n is index of the list that the element is contained in result 
;;;      and m is index of the element in the nth element of result,
;;;       (list-ref (list-ref result m) n) = (list-ref (list-ref columned-data-set n) m)
(define reformat-to-rows
  (lambda (columned-data-set)
    (let ([len (length (car columned-data-set))])
      (let kernel ([pos 0])
        (if (= pos len)
            null     
            (let ([row-list
                   (map (section list-ref <> pos)
                        columned-data-set)])
              (cons row-list
                    (kernel (increment pos)))))))))

;;; Procedure:
;;;   combine-indep-vars-with-diag
;;; Parameters:
;;;   diagnoses, a list
;;;   indep-vars, a nested list
;;; Purpose:
;;;   To merge diagnoses, bias and indep-vars (independent variables) in one table
;;; Produces:
;;;   result, a nested list
;;; Preconditions:
;;;   (length diagnoses) = (length indep-vars) 
;;; Postconditions:
;;;   * For every n that 0 <= n < (length result),
;;;    length of nth element in result is (length of nth element in indep-var) + 2)
;;;    nth element in result = (append (cons (list-ref diagnoses n) (cons 1 (list-ref indep-vars n))))
(define combine-indep-vars-with-diag
  (lambda (diagnoses indep-vars)
    (let kernel ([d-remaining diagnoses]
                 [i-remaining (map (section cons 1 <>)
                                   indep-vars)])
      (if (null? d-remaining)
          null
          (cons (cons (car d-remaining)
                      (car i-remaining))
                (kernel (cdr d-remaining)
                        (cdr i-remaining)))))))

;;; Final data set (clean, scaled) for training
(define final-data-set
  (let ([scaled-indep-vars (reformat-to-rows (scale-multiple-columns
                                              (just-the-indep-vars clean-data-set)))])
    (combine-indep-vars-with-diag actual-diagnoses scaled-indep-vars)))
        
; +----------+-----------------------------------------------------------
; | Training |
; +----------+

;;; Procedure:
;;;   new-weight
;;; Parameters:
;;;   w, old weight, a real number
;;;   g, guess, a real number
;;;   y, desired, an integer
;;;   z, weighted sum, a real number
;;;   a, data value, a real number
;;; Purpose:
;;;   To generate one updated weight from the weight vector
;;; Produces:
;;;   result, a real number
;;; Preconditions:
;;;   0<=g<=1
;;;   y=0 or y=1
;;; Postconditions:
;;;   * result = w-(2*(g-y)*((e^z)/(e^z+1)^2)*a)
;;;     ((e^z)/(e^z+1)^2) is derivative of sigmoid function which is (1/(1+ e^(-z)))
(define new-weight
  (lambda (w g y z a)
    (- w (* 2
            (- g y)
            (/ (exp z) (square (+ (exp z) 1)))
            a))))

;;; Procedure:
;;;   calculate-weighted-sum
;;; Parameters:
;;;   w-vec, a vector
;;;   entry, a list
;;; Purpose:
;;;   To generate weighted sum
;;; Produces:
;;;   result, a real number
;;; Preconditions:
;;;   every elements of w-vec and entry is real number
;;;   (length entry) = (vector-length w-vec)
;;;   entry is a list without a diagnosis information
;;; Postconditions:
;;;   * if the elements of w-vec are w0, w1, ... , wn
;;;      and the elements of entry are a0, a1, ... , an
;;;      result = w0*a0 + w1*a1 + ... + wn*an
(define calculate-weighted-sum
  (lambda (w-vec entry)
    (let kernel ([pos 0]
                 [remaining entry]
                 [so-far 0])
      (if (null? remaining)
          so-far
          (kernel (increment pos)
                  (cdr remaining)
                  (+ so-far (* (vector-ref w-vec pos) (car remaining))))))))

;;; Procedure:
;;;   update-weight!
;;; Parameters:
;;;   *w-vec, old weight vector, a vector
;;;   *g, guess, a real number
;;;   *y, desired, an integer
;;;   *z, weighted sum, a real number
;;;   *entry-w/diag, a list of real numbers from 'final-data-set'.
;;; Purpose:
;;;   To update all weights in w-vec
;;; Produces:
;;;   result, a vector
;;; Preconditions:
;;;   All elements of w-vec are real numbers
;;;   0<=g<=1
;;;   y=0 or y=1
;;; Postconditions:
;;;   * Let val be (vector-ref vec index) before the procedure call. After the
;;;     call (vector-ref vec index) produces (new-weight val g y z a).
(define update-weight!
  (lambda (w-vec entry-w/diag)
    (let* ([diag (car entry-w/diag)]
           [entry (cdr entry-w/diag)]
           [z (calculate-weighted-sum w-vec entry)]
           [g (/ 1 (+ 1 (exp (- 0 z))))])
      (let kernel ([pos 0]
                   [remaining entry])
        (when (< pos (vector-length w-vec))
          (vector-set! w-vec
                       pos
                       (new-weight (vector-ref w-vec pos) g diag z (car remaining)))
          (kernel (increment pos)
                  (cdr remaining)))))
    w-vec))

;;; Procedure:
;;;   train
;;; Parameters:
;;;   w-vec, a vector
;;;   data-set, a nested list
;;; Purpose:
;;;   To update the w-vec after considering every entry in data-set. 
;;; Produces:
;;;   result, a vector
;;; Preconditions:
;;;   * (length w-vec) = (length (list-ref data-set i))
;;;     where 0 <= i < (length data-set)
;;;   * every number in data-set is between -1 and 1 (inclusive)
;;; Postconditions:
;;;   * (length result) = (length w-vec)
;;;   * (list-ref result i) = (update-weight! (list-ref w-vec i)) 
(define train
  (lambda (w-vec data-set)
    (if (null? data-set)
        w-vec
        (train (update-weight! w-vec (car data-set))
               (cdr data-set)))))

;;; Procedure:
;;;   train-multiple
;;; Parameters:
;;;   trainings, a positive integer
;;;   w-vec, a vector
;;;   data-set, a nested list
;;; Purpose:
;;;   To update the w-vec after considering every entry in data-set trainings amount of times. 
;;; Produces:
;;;   result, a vector
;;; Preconditions:
;;;   * (length w-vec) = (length (list-ref data-set i))
;;;     where 0 <= i < (length data-set)
;;;   * every number in data-set is between -1 and 1 (inclusive)
;;; Postconditions:
;;;   * (train w-vec data-set) is ran trainings amount of times
;;;   * (length result) = (length w-vec)
;;;   * (list-ref result i) = (update-weight! (list-ref w-vec i)) 
(define train-multiple
  (lambda (trainings w-vec data-set)
    (when (not (zero? trainings))
      (train w-vec data-set)
      (train-multiple (decrement trainings) w-vec data-set))
    w-vec))

; +----------+-----------------------------------------------------------
; | Running  |
; +----------+

(define weight-vector (make-vector 21 1))

;;; Procedure:
;;;   diagnoser
;;; Parameters:
;;;   w-vec, a vector
;;;   entry-w/diag, a list
;;; Purpose:
;;;   Return M or B depending on whether a guess calculated based on the weight vector
;;;   is above or below 0.5 respectively
;;; Produces:
;;;   diagnosis, a string
;;; Preconditions:
;;;   * (length w-vec) = 21
;;;   * entry-w/diag must follow form of:
;;;      ((diagnosis) (bias) (val1) (val2) ... (val20))
;;; Postconditions:
;;;   * Returns "M" if guess is greater than 0.5 inclusive
;;;   * Returns "B" if guess is less than 0.5
;;;   * Guess is calculated via (/ 1 (+ 1 (exp (- 0 z))))
;;;   * Z = (+ (* weight1 entry1) (* weight2 entry2) ... (*weight21 entry21))
(define diagnoser
  (lambda (w-vec entry-w/diag)
    (let* ([entry (cdr entry-w/diag)]
           [z (calculate-weighted-sum w-vec entry)]
           [g (/ 1 (+ 1 (exp (- 0 z))))])
      (if (>= g 0.5)
          "M"
          "B"))))

;;; Procedure:
;;;   diagnoser-binary
;;; Parameters:
;;;   w-vec, a vector
;;;   entry-w/diag, a list
;;; Purpose:
;;;   Return 1 or 0 depending on whether a guess calculated based on the weight vector
;;;   is above or below 0.5 respectively
;;; Produces:
;;;   diagnosis, a string
;;; Preconditions:
;;;   * (length w-vec) = 21
;;;   * entry-w/diag must follow form of:
;;;      ((diagnosis) (bias) (val1) (val2) ... (val20))
;;; Postconditions:
;;;   * Returns 1 if guess is greater than 0.5 inclusive
;;;   * Returns 0 if guess is less than 0.5
;;;   * Guess is calculated via (/ 1 (+ 1 (exp (- 0 z))))
;;;   * Z = (+ (* weight1 entry1) (* weight2 entry2) ... (*weight21 entry21))
(define diagnoser-binary
  (lambda (w-vec entry-w/diag)
    (let* ([entry (cdr entry-w/diag)]
           [z (calculate-weighted-sum w-vec entry)]
           [g (/ 1 (+ 1 (exp (- 0 z))))])
      (if (>= g 0.5)
          1
          0))))

;;; Procedure:
;;;   accuracy
;;; Parameters:
;;;   data-set, a nested list
;;; Purpose:
;;;   Using a trained weight vector and data, return a decimal representing
;;;   a percentage of how many diagnoses the diagnoser correctly guessed
;;; Produces:
;;;   result, a number
;;; Preconditions:
;;;   * Each entry of data-set must follow form of final-data-set:
;;;     ((diagnosis) (bias) (val1) (val2) ... (val20))
;;;   * For all valid index of data-set, n, (length (list-ref data-set n)) = 22
;;;   * Each element of all entries of data-set are numbers
;;; Postconditions:
;;;   * result is inexact
;;;   * 0 <= result <= 1
;;;   * result represents a percentage
(define accuracy
  (lambda (data-set)
    (let* ([guess-diagnostics (map (section diagnoser-binary weight-vector <>) data-set)]
           [compare-diagnostics (map equal? guess-diagnostics actual-diagnoses)]
           [tallies (tally-all compare-diagnostics)]
           [total-guesses (+ (cadr (car tallies))
                             (cadr (cadr tallies)))])
      (* 100
         (exact->inexact (/ (- total-guesses
                               (cadr (cadr tallies)))
                            total-guesses))))))

; +-------------+-----------------------------------------------------------
; | Conclusion  |
; +-------------+

;The primary goal of our project was to create a program that made malignancy diagnoses
;with accuracy by considering past incidents of cancer. We used records of 569 cases of
;breast cancer in the state of Wisconsin which included the diagnosis as well as quantitative
;characteristics about the tumors. To implement this program, we used the perceptron machine-
;learning algorithm which involves calculating a weighted sum and improving it by using the
;gradient of a cost function.

;In short, our project was a great success. Our program was able to correctly classify cancer cells
;as malignant or benign 97.4% of the time. Furthermore, the set of weights that was created by
;our program (our final weight vector) lends insight into what cell characteristics are more
;important in diagnosing a malignant tumor. We learned two things. First, the average values and
;the outlier values for each characteristic were approximately the same in terms of their usefulness
;to the algorithm. Second, the three most important characteristics for considering the malignancy of
;the tumor are the cell's radius, perimeter, and fractal dimension. These conclusions can be made
;without any specific knowledge about the specifics of the biology involved. 