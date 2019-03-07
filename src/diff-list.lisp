(defpackage #:bindle.diff-list
  (:documentation "A functional diff list using continuations. Allows O(1) append and can be
into a classic list whenever needed")
  (:use #:cl)
  (:shadowing-import-from #:cl #:member)
  (:export #:
           #:d-cons
           #:d-append
           #:d-snoc
           #:to-list
           #:+empty+))

(in-package #:bindle.diff-list)

(defparameter +empty+
  (lambda (k) k)) ; an empty difference list is equivalent to
             ; the identity function

(defun d-cons (x cont)
  ; to cons a difference list we must first apply the
  ; continuation `cont` to a valid list but we haven't
  ; got one yet. Thus we delay the construction by
  ; instead returning a lambda function binding `k`
  ; which `cont` is then later applied to. This lambda
  ; is a continuation.
  (lambda (k)
    (cons x ; the scalar value to be joined
          (funcall cont k)))) ; application of the continuation


  ; append of two difference lists is the application
  ; of the continuation `cont-x` to the result of
  ; applying the continuation `cont-y` to `k`. `k` is
  ; given by the lambda binding thus this is also
  ; just another continuation.
(defun d-append (cont-x cont-y)
  (lambda (k)
    (funcall cont-x (funcall cont-y k))))

(defun d-snoc (cont x)
  ; to add a value at the end we construct another
  ; continuation but this time fill in before `cont`
  (lambda (k)
    (funcall cont (cons x k))))

(defun to-list (cont)
  ; to "extract" the list from the continuation form
  ; we apply the continuation to the empty list
  ; which then fills in `k` at the end. This is where
  ; we pay the cost of diff-append, diff-cons, and
  ; diff-snoc by having to traverse the whole list to
  ; add '() in place of `k` at the end.
  (funcall cont '()))

(defun of-list (lis)
    (reduce #'d-cons lis
            :initial-value +empty+
            :from-end t))
