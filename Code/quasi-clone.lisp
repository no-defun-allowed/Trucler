(cl:in-package #:trucler)

(defun quasi-clone (instance &rest arguments)
  (apply #'make-instance
         (class-of instance)
         (append arguments
                 (loop for (initarg reader) in (cloning-information instance)
                       collect initarg
                       collect (funcall reader instance)))))
