(defpackage cl-morse
  (:use :cl :cl-ppcre)
  (:export :unknown-character
           :string->morse         
           :morse->string))
(in-package :cl-morse)

;;; helpers

; The letters are kept as strings because I plan to add special signalling
; sequences in the future
(defvar *morselist*
  '(("a" ".-/")
    ("b" "-.../")
    ("c" "-.-./")
    ("d" "-../")
    ("e" "./")
    ("f" "..-./")
    ("g" "--./")
    ("h" "..../")
    ("i" "../")
    ("j" ".---/")
    ("k" "-.-/")
    ("l" ".-../")
    ("m" "--/")
    ("n" "-./")
    ("o" "---/")
    ("p" ".--./")
    ("q" "--.-/")
    ("r" ".-./")
    ("s" ".../")
    ("t" "-/")
    ("u" "..-/")
    ("v" "...-/")
    ("w" ".--/")
    ("x" "-..-/")
    ("y" "-.--/")
    ("z" "--../")
    (" " "/")
    ("." "//"))
  "List of tuples alpha morse.")

(defvar *alpha->morse* (make-hash-table :test #'equalp)
  "Hashtable mapping alpha strings to morse code. Not case sensitive.")

(defvar *morse->alpha* (make-hash-table :test #'equalp)
  "Hashtable mapping morse code string to alpha.")

(defun init-hashtables ()
  "Initializes *alpha->morse* and *morse->alpha* hashtables from values
   in morselist"
  (dolist (c *morselist*)
    (destructuring-bind (alpha morse) c
      (setf (gethash alpha *alpha->morse*) morse)
      (setf (gethash morse *morse->alpha*) alpha))))

;; initialize upon load
(init-hashtables)


;;; Converting string to morse
(define-condition unknown-character (error)
  ((ch :initarg :ch
       :reader unknown-ch
       :documentation "The unknown character itself."))
  (:report (lambda (condition stream)
             (format stream "Unknown charachter '~a', could not be converted to morse code."
                     (unknown-ch condition))))
  (:documentation "Is signalled when a character is not known and could not be converted to a morse code representation. The character is accessible through 'unknown-ch reader."))

(defun char->morse (c)
  "Converts a single character to a morse code. If character could not be
converted, signals an error 'unknown-character. 
Offers a restart 'skip-character which ignores the character and returns
an empty string."
  (restart-case
      (let ((morse (gethash (string c) *alpha->morse*)))
        (if (null morse)
            (error 'unknown-character :ch c)
            morse))
    (skip-character () "")))


(defun string->morse (str &key (skip-unknown-chars))
  "Converts the string to a morse code. If :skip-unknown-chars is true,
ignores characters which cannot be converted to a morse code. Otherwise signals
'unknown-character condition with unknown-ch reader to get the unknown character."
  (handler-bind ((unknown-character #'(lambda (c)
                                        (declare (ignore c))
                                        (when skip-unknown-chars 
                                          (invoke-restart 'skip-character)))))
    (let* ((lowercase    (coerce (string-downcase str) 'list))
           (morse-tokens (mapcar #'char->morse lowercase)))
      (apply #'concatenate 'string morse-tokens))))


;;; Converting morse to strings
(defun tokenize-string (str)
  "Returns a list of strings which are morse tokens. The strings are shared with the
   `str` parameter. If str cannot be tokenized, returns NULL."
  ;; regex "([\.\-]+\/|\/\/|\/)"
  ;; eihter: dots and dashes followed up by slash
  ;;     or: two slashes (full stop)
  ;;     or: signle slash (space)
  (ppcre:all-matches-as-strings "([\.-]+\/|\/\/|\/)" str :sharedp t))

(defun morse->char (morsechr)
  "Converts one morse code character to alpha character. If character could not be converted, signals an error 'unknown-character.
Offers a restart 'skip-character which ignorese the character and returns an empty string."
  (restart-case
      (let ((chr (gethash morsechr *morse->alpha*)))
        (if (null chr)
            (error 'unknown-character :ch chr)
            chr))
    (skip-character () "")))


(defun morse->string (morsestr &key (skip-unknown-chars))
  "Converts the string with morse code to string with alpha characters."
  (handler-bind ((unknown-character #'(lambda (c)
                                        (declare (ignore c))
                                        (when skip-unknown-chars
                                          (invoke-restart 'skip-character)))))
    (let* ((tokens    (tokenize-string morsestr))
           (conv-list (mapcar #'morse->char tokens)))
      (apply 'concatenate 'string conv-list))))



;; blah blah blah.
