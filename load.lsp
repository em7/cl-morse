
;; My script to load this system:
(require :asdf)
(asdf:load-asd #p"c:/Users/mader/Documents/src/cl-morse/cl-morse.asd")
(asdf:load-system :cl-morse)

(in-package :cl-morse)

