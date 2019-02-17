(defpackage #:bindle.set
  (:documentation "Gives a functional set that has quick copies. Useful for removing
and adding local bindings")
  (:use #:cl)
  (:shadowing-import-from #:cl #:member)
  (:export #:mem
           #:add
           #:add-many
           #:add-seq
           #:fset
           #:+empty+))

(in-package #:bindle.set)

(deftype fset ()
  `list)


;; Red and Black Set----------------------------------------------------------------------
(defconstant +red+ :red)
(defconstant +black+ :black)

(defconstant +empty+ :rb-Empty)

(deftype color ()
  "colors for a red-black tree"
  `(member ,+red+ ,+black+))

(deftype red-black ()
  `(or (eql :rb-Empty)
      (satisfies rb-tree-p)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (rb-tree (:conc-name t-))
    (col +black+ :type color)
    elem
    (left +empty+ :type red-black)
    (right +empty+ :type red-black)))

(defun red-black-p (tree)
  (typep tree 'red-black))

;; Functions------------------------------------------------------------------------------

(defun mem (ele set)
  (cond ((eq +empty+ set)                nil)
        ((less-than    ele (t-elem set)) (mem ele (t-left set)))
        ((greater-than ele (t-elem set)) (mem ele (t-right set)))
        (t t)))

;; Ι really wish I had pattern matching right now :(
;; this is what macros should save us from!!!
;; a b c d are trees
;; x y and z are values
;; more precisely a = left-left
;;                b = left-right
;;                c = right-left
;;                d = right-right
;;                x = left-ele
;;                y = main-ele
;;                z = right-ele
(defun balance (color left val right)
  (labels ((make-tree (a x b y c z d)
             (make-rb-tree :col +red+
                           :left (make-rb-tree :col :black :left a :elem x :right b)
                           :elem y
                           :right (make-rb-tree :col :black :left c :elem z :right d)))
           ;; flip the first two arguments to the end
           (make-tree-r (b y c z d x a)
             (make-tree a x b y c z d))
           (check-left (x)
             (and (rb-tree-p x)
                  (eq +red+ (t-col x))
                  (rb-tree-p (t-left x))
                  (eq +red+ (t-col (t-left x)))))
           (check-right (x)
             (and (rb-tree-p x)
                  (eq +red+ (t-col x))
                  (rb-tree-p (t-right x))
                  (eq +red+ (t-col (t-right x)))))
           (construct-left (f x y)
             (funcall f (t-left  (t-left x))
                        (t-elem  (t-left x))
                        (t-right (t-left x))
                        (t-elem  x)
                        (t-right x)
                        val
                        y))
           (construct-right (f x y)
             (funcall f (t-left x)
                        (t-elem x)
                        (t-left  (t-right x))
                        (t-elem  (t-right x))
                        (t-right (t-right x))
                        val
                        y)))
    (cond
      ((eq color +red+)    (make-rb-tree :col color :left left :elem val :right right))
      ((check-left left)   (construct-left   #'make-tree left right))
      ((check-right left)  (construct-right  #'make-tree left right))
      ((check-left right)  (construct-left   #'make-tree-r right left))
      ((check-right right) (construct-right  #'make-tree-r right left))
      (t                   (make-rb-tree :col color :left left :elem val :right right)))))

(defun add (val tree)
  (labels ((ins (tree)
             (cond ((eq +empty+ tree)
                    (make-rb-tree :col +red+ :elem val))
                   ((less-than val (t-elem tree))
                    (balance (t-col tree)
                             (ins (t-left tree))
                             (t-elem tree)
                             (t-right tree)))
                   ((greater-than val (t-elem tree))
                    (balance (t-col tree)
                             (t-left tree)
                             (t-elem tree)
                             (ins (t-right tree))))
                   (t tree))))
    (let ((new-tree (ins tree)))
      (setf (t-col new-tree) +black+)
      new-tree)))


(defun add-many (tree &rest list)
  (reduce (lambda (acc x) (add x acc)) list :initial-value tree))

(defun add-seq (seq tree)
  (apply #'add-many tree seq))

(defun to-list (tree)
  (if (eq +empty+ tree)
      '()
      (append (to-list (t-left tree))
              (cons (t-elem tree)
                    (to-list (t-right tree))))))
;; Generic comparitors--------------------------------------------------------------------

(defgeneric less-than (a b)
  (:documentation "used to create a generic comparison function"))

(defmethod less-than ((a number) (b number))
  (< a b))

(defmethod less-than ((a t) (b t))
  (< (sxhash a) (sxhash b)))

(defun greater-than (a b)
  "A generic comparison function that just calls less-than, so extend
   greater-than by making another method for less-than"
  (less-than b a))
