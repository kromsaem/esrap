;;;; ESRAP -- a packrat parser for Common Lisp - builtin grammars
;;;; by Jan Moringen, 2012
;;;;
;;;; See esrap.lisp for licence and other information.

(in-package :esrap)

;;; Whitespace grammar

(defgrammar #:whitespace)
(in-grammar #:whitespace)

(defrule whitespace?
    (* (or #\Space #\Tab #\Newline))
  (:constant nil))

(defrule whitespace
    (+ (or #\Space #\Tab #\Newline))
  (:constant nil))

(defrule whitespace/not-newline?
    (* (or #\Space #\Tab))
  (:constant nil))

(defrule whitespace/not-newline
    (+ (or #\Space #\Tab))
  (:constant nil))

(defrule same-line
    (* (not #\Newline))
  (:text t))

;;; Comments grammar

(defgrammar #:comments
  (:use #:whitespace))
(in-grammar #:comments)

;; C-style comments

(defrule comment/c-style/rest-of-line
    (and (and #\/ #\/) same-line (? #\Newline))
  (:destructure (open content close)
    (declare (ignore open close))
    (string-trim '(#\/ #\Tab #\Space) content)))

(defrule comment-content/c-style/delimited
    (* (or (not #\*) (and #\* (! #\/))))
  (:lambda (content)
    (let* ((text  (string-trim '(#\* #\Tab #\Space #\Newline) (text content)))
           (lines (split-sequence #\Newline text)))
      (format nil "~{~A~^~%~}"
              (mapcar (curry #'string-left-trim '(#\* #\Tab #\Space))
                      lines)))))

(defrule comment/c-style/delimited
    (and (and #\/ #\*) comment-content/c-style/delimited (and #\* #\/))
  (:function second))

(defrule comment/c-style
    (or comment/c-style/rest-of-line comment/c-style/delimited))

;; Shell-style comments

(defrule comment/shell-style
    (and #\# same-line (? #\Newline))
  (:destructure (hash content newline)
    (declare (ignore hash newline))
    (string-left-trim '(#\#) content)))

;; Lisp-style comments

(defrule comment/lisp-style
    (and #\; same-line (? #\Newline))
  (:destructure (comma content newline)
    (declare (ignore comma newline))
    (string-left-trim '(#\;) content)))

;;; Literals grammar

(defgrammar #:literals)
(in-grammar #:literals)

(defrule digit
    (digit-char-p character))

(macrolet ((define-string-literal-rule (name delimiter)
             (let ((escape (symbolicate name '#:/escape)))
               `(progn
                  (defrule ,escape
                      (and #\\ (or ,delimiter #\\))
                    (:function second))

                  (defrule ,name
                      (and ,delimiter
                           (* (or ,escape (not ,delimiter)))
                           ,delimiter)
                    (:destructure (open content close)
                      (declare (ignore open close))
                      (text content)))))))

  (define-string-literal-rule string/double-quotes #\")
  (define-string-literal-rule string/single-quotes #\'))
