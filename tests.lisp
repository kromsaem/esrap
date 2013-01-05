;;;;  Copyright (c) 2007-2012 Nikodemus Siivola <nikodemus@sb-studio.net>
;;;;
;;;;  Permission is hereby granted, free of charge, to any person
;;;;  obtaining a copy of this software and associated documentation files
;;;;  (the "Software"), to deal in the Software without restriction,
;;;;  including without limitation the rights to use, copy, modify, merge,
;;;;  publish, distribute, sublicense, and/or sell copies of the Software,
;;;;  and to permit persons to whom the Software is furnished to do so,
;;;;  subject to the following conditions:
;;;;
;;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;;  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;;  CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;;  TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;;  SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(in-package :cl-user)

(defpackage :esrap-tests
  (:use :alexandria :cl :esrap :eos)
  (:shadowing-import-from :esrap "!")
  (:export #:run-tests))

(in-package :esrap-tests)

;;; Some helper macros

(defmacro with-clean-outer-compilation-unit (&body body)
  ;; Keep outer compilation-unit report clean.
  `(let ((*error-output*    (make-broadcast-stream)))
     (with-compilation-unit (:override t)
       ,@body)))

(defmacro ignoring-compile-time-warnings ((condition) &body body)
  `(funcall
    (with-clean-outer-compilation-unit
        (handler-bind
            ((,condition #'muffle-warning))
          (compile nil '(lambda () ,@body))))))

(defmacro compilation-signals (condition &body body)
  `(with-clean-outer-compilation-unit
       (signals ,condition
         (compile nil '(lambda () ,@body)))))

(defun defgrammar-test.use-non-existent-grammar ()
  ;; We should see a warning at compile time and an error at
  ;; runtime. Check both independently.

  ;; Compile to check full warning at compile time.
  (compilation-signals grammar-not-found-warning
    (defgrammar #:baz (:use #:does-not-exist)))

  ;; Compile, ignoring compile time warnings, then check runtime
  ;; error.
  (signals grammar-not-found-error
    (ignoring-compile-time-warnings (grammar-not-found-warning)
      (defgrammar #:baz (:use #:does-not-exist)))))

(defun in-grammar-test.non-existent-grammar ()
  ;; See defgrammar-test.use-non-existent.
  (compilation-signals grammar-not-found-warning
    (in-grammar #:does-not-exist))

  (signals grammar-not-found-error
    (ignoring-compile-time-warnings (grammar-not-found-warning)
      (in-grammar #:does-not-exist))))

(defun find-grammar-test.non-existent ()
  (is (null (find-grammar '#:does-not-exist :if-does-not-exist nil)))
  (signals grammar-not-found-warning
    (find-grammar '#:does-not-exist :if-does-not-exist #'warn))
  (signals grammar-not-found-error
    (find-grammar '#:does-not-exist :if-does-not-exist #'error)))

(defun defrule-test.non-existent-grammar ()
  ;; See defgrammar-test.use-non-existent.
  (compilation-signals grammar-not-found-warning
    (defrule (foo :grammar #:does-not-exist) "foo"))

  (signals grammar-not-found-error
    (ignoring-compile-time-warnings (grammar-not-found-warning)
      (defrule (foo :grammar #:does-not-exist) "foo"))))

(defgrammar #:find-rule-test.non-existent.empty)

(defun find-rule-test.non-existent ()
  (is (null (find-rule '#:find-rule-test.non-existent.empty '#:does-not-exist
                       :if-does-not-exist nil)))
  (signals rule-not-found-warning
    (find-rule '#:find-rule-test.non-existent.empty '#:does-not-exist
               :if-does-not-exist #'warn))
  (signals rule-not-found-error
    (find-rule '#:find-rule-test.non-existent.empty '#:does-not-exist
               :if-does-not-exist #'error)))

;; We define three rules across two grammars:
;; Grammar FOO:         B -uses-> A
;; Grammar BAR: C -uses-^
;; Then try remove rules A and B in different ways.
(defgrammar #:remove-rule-test.used.foo)
(defgrammar #:remove-rule-test.used.bar
  (:use #:remove-rule-test.used.foo))

(defun define-rules-for-used-test ()
  (defrule (a :grammar #:remove-rule-test.used.foo) (+ "a"))
  (defrule (b :grammar #:remove-rule-test.used.foo) a)
  (defrule (c :grammar #:remove-rule-test.used.bar) b))

(defun remove-rule-test.used.error ()
  (define-rules-for-used-test)

  ;; A in FOO is used by B in FOO.
  (signals error (remove-rule '#:remove-rule-test.used.foo 'a))
  ;; B in FOO is used by C in BAR.
  (signals error (remove-rule '#:remove-rule-test.used.foo 'b)))

(defun remove-rule-test.used.correct-order ()
  (define-rules-for-used-test)

  ;; This order should work.
  (remove-rule '#:remove-rule-test.used.bar 'c)
  (remove-rule '#:remove-rule-test.used.foo 'b)
  (remove-rule '#:remove-rule-test.used.foo 'a))

(defun remove-rule-test.used.redefinition ()
  (define-rules-for-used-test)

  ;; Redefine B in FOO to not use A in FOO, then delete A in FOO.
  (defrule (b :grammar #:remove-rule-test.used.foo) (or))
  (remove-rule '#:remove-rule-test.used.foo 'a)

  ;; Redefine C in BAR to not use B in FOO, then delete B in FOO.
  (defrule (c :grammar #:remove-rule-test.used.bar) (or))
  (remove-rule '#:remove-rule-test.used.foo 'b)

  ;; C in BAR can be removed without redefinition.
  (remove-rule '#:remove-rule-test.used.bar 'c))

;; We define two grammars, FOO and BAR, and make sure that BAR can use
;; rules defined in FOO.
(defgrammar #:foo)

(defrule (a :grammar #:foo)
    (+ "a"))

(defgrammar #:bar
  (:use #:foo))

(defrule (b :grammar #:bar)
    (or a (+ "b"))
  (:text t))

(defun grammar-inclusion-test.smoke ()
  (is (string= (parse '#:bar 'b "aa") "aa"))
  (is (string= (parse '#:bar 'b "bb") "bb")))

;; We define a minimal grammar now, to avoid compile-time warning. At
;; runtime, we delete everything and set it up again. This is required
;; for repeated execution of the test.
(defgrammar #:fez)
(defrule (d :grammar #:fez) (or))

(defun grammar-inclusion-test.redefinition-accross-grammars ()
  ;; We define two grammars, BAZ and FEZ, and make sure that FEZ can use
  ;; rules defined in BAZ. Grammar FEZ uses rules A and B from BAZ. Rule
  ;; B is undefined in both grammars at first and defined in BAZ later.
  (eval '(progn
          (defrule (d :grammar #:fez) (or))

          (defgrammar #:baz)
          (remove-rule '#:baz 'b)
          (defrule (a :grammar #:baz)
              (+ "a"))

          (defgrammar #:fez (:use #:baz))
          (defrule (d :grammar #:fez)
              (or a (+ "c") b)
            (:text t))))

  ;; Before redefinition:
  (is (string= (parse '#:fez 'd "aa") "aa"))
  (signals rule-not-found-error (parse '#:fez 'd "AA"))
  (is (string= (parse '#:fez 'd "cc") "cc"))
  (signals rule-not-found-error (parse '#:fez 'd "bb"))

  ;; After redefinition of A in BAZ:
  (format t "Defining ~S in ~A~%"
          'a  '#:baz)
  (eval '(defrule (a :grammar #:baz) (+ "A")))

  (signals rule-not-found-error (parse '#:fez 'd "aa") "aa")
  (is (string= (parse '#:fez 'd "AA") "AA"))
  (is (string= (parse '#:fez 'd "cc") "cc"))
  (signals rule-not-found-error (parse '#:fez 'd "bb"))

  ;; After redefinition of B in BAZ:
  (format t "Defining ~S in ~A~%"
          'b  '#:baz)
  (eval '(defrule (b :grammar #:baz) (+ "b")))

  (signals esrap-error (parse '#:fez 'd "aa") "aa")
  (is (string= (parse '#:fez 'd "AA") "AA"))
  (is (string= (parse '#:fez 'd "cc") "cc"))
  (is (string= (parse '#:fez 'd "bb") "bb")))

;; grammar-inclusion-test.redefinition-with-added-use

(defgrammar #:grammar-inclusion-test.redefinition-with-added-use.foo)
(defrule (a :grammar #:grammar-inclusion-test.redefinition-with-added-use.foo)
    "a")
(defgrammar #:grammar-inclusion-test.redefinition-with-added-use.bar)
(defrule (b :grammar #:grammar-inclusion-test.redefinition-with-added-use.bar)
    a)

(defun grammar-inclusion-test.redefinition-with-added-use ()
  (signals rule-not-found-error
    (parse '#:grammar-inclusion-test.redefinition-with-added-use.bar 'b "a"))

  (defgrammar #:grammar-inclusion-test.redefinition-with-added-use.bar
    (:use #:grammar-inclusion-test.redefinition-with-added-use.foo))

  (is (string= (parse '#:grammar-inclusion-test.redefinition-with-added-use.bar 'b "a") "a")))

;; grammar-inclusion-test.redefinition-with-removed-use

(defgrammar #:grammar-inclusion-test.redefinition-with-removed-use.foo)
(defrule (a :grammar #:grammar-inclusion-test.redefinition-with-removed-use.foo)
    "a")
(defgrammar #:grammar-inclusion-test.redefinition-with-removed-use.bar
  (:use #:grammar-inclusion-test.redefinition-with-removed-use.foo))
(defrule (b :grammar #:grammar-inclusion-test.redefinition-with-removed-use.bar)
    a)

(defun grammar-inclusion-test.redefinition-with-removed-use ()
  (is (string= (parse '#:grammar-inclusion-test.redefinition-with-removed-use.bar 'b "a") "a"))

  (defgrammar #:grammar-inclusion-test.redefinition-with-removed-use.bar)

  (signals rule-not-found-error
    (parse '#:grammar-inclusion-test.redefinition-with-removed-use.bar 'b "a")))

(defun grammar-inclusion-test.circular-dependency ()
  "TODO(jmoringe): document"
  (eval '(progn
          ;; Clear uses in case the test is run multiple times.
          (defgrammar #:grammar-inclusion-test.circular-dependency.a)
          (defgrammar #:grammar-inclusion-test.circular-dependency.b)

          ;; Now setup the circular dependency.
          (defgrammar #:grammar-inclusion-test.circular-dependency.b
            (:use #:grammar-inclusion-test.circular-dependency.a))
          (signals simple-error
            (defgrammar #:grammar-inclusion-test.circular-dependency.a
              (:use #:grammar-inclusion-test.circular-dependency.b))))))

(defgrammar #:test)
(in-grammar #:test)

;;;; A few semantic predicates

(defun not-doublequote (char)
  (not (eql #\" char)))

(defun not-digit (char)
  (when (find-if-not #'digit-char-p char)
    t))

(defun not-newline (char)
  (not (eql #\newline char)))

(defun not-space (char)
  (not (eql #\space char)))

;;;; Utility rules

(defrule whitespace (+ (or #\space #\tab #\newline))
  (:text t))

(defrule empty-line #\newline
  (:constant ""))

(defrule non-empty-line (and (+ (not-newline character)) (? #\newline))
  (:destructure (text newline)
    (declare (ignore newline))
    (text text)))

(defrule line (or empty-line non-empty-line)
  (:identity t))

(defrule trimmed-line line
  (:lambda (line)
    (string-trim '(#\space #\tab) line)))

(defrule trimmed-lines (* trimmed-line)
  (:identity t))

(defrule digits (+ (digit-char-p character))
  (:text t))

(defrule integer (and (? whitespace)
                      digits
                      (and (? whitespace) (or (& #\,) (! character))))
  (:destructure (whitespace digits tail)
    (declare (ignore whitespace tail))
    (parse-integer digits)))

(defrule list-of-integers (+ (or (and integer #\, list-of-integers)
                                 integer))
  (:destructure (match)
    (if (integerp match)
        (list match)
        (destructuring-bind (int comma list) match
          (declare (ignore comma))
          (cons int list)))))

(defun parse-test.smoke ()
  (is (equal '("1," "2," "" "3," "4.")
             (parse '#:test 'trimmed-lines "1,
                                            2,

                                            3,
                                            4.")))
  (is (eql 123 (parse '#:test 'integer "  123")))
  (is (eql 123 (parse '#:test 'integer "  123  ")))
  (is (eql 123 (parse '#:test 'integer "123  ")))
  (is (equal '(123 45 6789 0) (parse '#:test 'list-of-integers "123, 45  ,   6789, 0")))
  (is (equal '(123 45 6789 0) (parse '#:test 'list-of-integers "  123 ,45,6789, 0  "))))

(defun parse-test.non-existent-grammar ()
  ;; See defgrammar-test.use-non-existent.
  (compilation-signals grammar-not-found-warning
    (parse '#:does-not-exist '#:does-not-matter "foo"))

  (signals grammar-not-found-error
    (ignoring-compile-time-warnings (grammar-not-found-warning)
      (parse '#:does-not-exist '#:does-not-matter "foo"))))

(defgrammar #:parse-test.non-existent-rule.empty)

(defun parse-test.non-existent-rule ()
  ;; See defgrammar-test.use-non-existent.
  (compilation-signals rule-not-found-warning
    (parse '#:parse-test.non-existent-rule.empty '#:does-not-exist
           "foo"))
  (compilation-signals rule-not-found-warning
    (parse '#:parse-test.non-existent-rule.empty
           '(+ (or "a" #:does-not-exist))
           "foo"))

  (signals rule-not-found-error
    (ignoring-compile-time-warnings (grammar-not-found-warning)
      (parse '#:parse-test.non-existent-rule.empty '#:does-not-exist
             "foo")))
  (signals rule-not-found-error
    (ignoring-compile-time-warnings (grammar-not-found-warning)
      (parse '#:parse-test.non-existent-rule.empty
             '(+ (or "a" #:does-not-exist))
             "foo"))))

(defrule single-token/bounds.1 (+ (not-space character))
  (:lambda (result &bounds start end)
    (format nil "~A[~S-~S]" (text result) start end)))

(defrule single-token/bounds.2 (and (not-space character) (* (not-space character)))
  (:destructure (first &rest rest &bounds start end)
    (format nil "~C~A(~S-~S)" first (text rest) start end)))

(defrule tokens/bounds.1 (and (? whitespace)
                              (or (and single-token/bounds.1 whitespace tokens/bounds.1)
                                  single-token/bounds.1))
  (:destructure (whitespace match)
    (declare (ignore whitespace))
    (if (stringp match)
        (list match)
        (destructuring-bind (token whitespace list) match
          (declare (ignore whitespace))
          (cons token list)))))

(defrule tokens/bounds.2 (and (? whitespace)
                              (or (and single-token/bounds.2 whitespace tokens/bounds.2)
                                  single-token/bounds.2))
  (:destructure (whitespace match)
    (declare (ignore whitespace))
    (if (stringp match)
        (list match)
        (destructuring-bind (token whitespace list) match
          (declare (ignore whitespace))
          (cons token list)))))

(defrule left-recursion (and left-recursion "l"))

(defun bounds-test.1 ()
  (is (equal '("foo[0-3]")
             (parse '#:test 'tokens/bounds.1 "foo")))
  (is (equal '("foo[0-3]" "bar[4-7]" "quux[11-15]")
             (parse '#:test 'tokens/bounds.1 "foo bar    quux"))))

(defun bounds-test.2 ()
  (is (equal '("foo(0-3)")
             (parse '#:test 'tokens/bounds.2 "foo")))
  (is (equal '("foo(0-3)" "bar(4-7)" "quux(11-15)")
             (parse '#:test 'tokens/bounds.2 "foo bar    quux"))))

(defun condition-test.1 ()
  (macrolet
      ((signals-esrap-error ((input position &optional messages) &body body)
         `(progn
            (signals (esrap-error)
              ,@body)
            (handler-case (progn ,@body)
              (esrap-error (condition)
                (is (string= (esrap-error-text condition) ,input))
                (is (= (esrap-error-position condition) ,position))
                ,@(when messages
                    `((let ((report (princ-to-string condition)))
                        ,@(mapcar (lambda (message)
                                    `(is (search ,message report)))
                                  (ensure-list messages))))))))))
    (signals-esrap-error ("" 0 ("Could not parse subexpression"
                                "Encountered at"))
      (parse '#:test 'integer ""))
    (signals-esrap-error ("123foo" 3 ("Could not parse subexpression"
                                      "Encountered at"))
      (parse '#:test 'integer "123foo"))
    (signals-esrap-error ("1, " 1 ("Incomplete parse."
                                   "Encountered at"))
      (parse '#:test 'list-of-integers "1, "))))

(defun condition-test.2 ()
  (signals (left-recursion)
    (parse '#:test 'left-recursion "l"))
  (handler-case (parse '#:test 'left-recursion "l")
    (left-recursion (condition)
      (is (string= (esrap-error-text condition) "l"))
      (is (= (esrap-error-position condition) 0))
      (is (eq (left-recursion-nonterminal condition)
              'left-recursion))
      (is (equal (left-recursion-path condition)
                 '(left-recursion left-recursion))))))

(defun negation-test ()
  (let* ((text "FooBazBar")
         (t1c (text (parse '#:test '(+ (not "Baz")) text :junk-allowed t)))
         (t1e (text (parse '#:test (identity '(+ (not "Baz"))) text :junk-allowed t)))
         (t2c (text (parse '#:test '(+ (not "Bar")) text :junk-allowed t)))
         (t2e (text (parse '#:test (identity '(+ (not "Bar"))) text :junk-allowed t)))
         (t3c (text (parse '#:test '(+ (not (or "Bar" "Baz"))) text :junk-allowed t)))
         (t3e (text (parse '#:test (identity '(+ (not (or "Bar" "Baz")))) text :junk-allowed t))))
    (is (equal "Foo" t1c))
    (is (equal "Foo" t1e))
    (is (equal "FooBaz" t2c))
    (is (equal "FooBaz" t2e))
    (is (equal "Foo" t3c))
    (is (equal "Foo" t3e))))

(test esrap

  (defgrammar-test.use-non-existent-grammar)
  (in-grammar-test.non-existent-grammar)
  (find-grammar-test.non-existent)

  (defrule-test.non-existent-grammar)
  (find-rule-test.non-existent)

  (remove-rule-test.used.error)
  (remove-rule-test.used.correct-order)
  (remove-rule-test.used.redefinition)

  (grammar-inclusion-test.smoke)
  (grammar-inclusion-test.redefinition-accross-grammars)
  (grammar-inclusion-test.redefinition-with-added-use)
  (grammar-inclusion-test.redefinition-with-removed-use)
  (grammar-inclusion-test.circular-dependency)

  (parse-test.smoke)
  (parse-test.non-existent-grammar)
  (parse-test.non-existent-rule)
  (bounds-test.1)
  (bounds-test.2)
  (condition-test.1)
  (condition-test.2)
  (negation-test)

  )

(defun run-tests ()
  (let ((results (run 'esrap)))
    (eos:explain! results)
    (unless (eos:results-status results)
      (error "Tests failed."))))
