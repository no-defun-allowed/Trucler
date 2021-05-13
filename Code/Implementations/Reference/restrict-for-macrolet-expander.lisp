(cl:in-package #:trucler-reference)

(defgeneric unavailable-under-restriction-p (description))

(defmethod unavailable-under-restriction-p (description)
  nil)

(macrolet ((unavailable-under-restriction (&rest names)
             `(progn
                ,@(loop for name in names
                        collect `(defmethod unavailable-under-restriction-p
                                     ((description ,name))
                                   t)))))
  (unavailable-under-restriction
   trucler:lexical-variable-description
   trucler:special-variable-description
   trucler:local-function-description
   trucler:block-description
   trucler:tag-description))

(defmethod trucler:restrict-for-macrolet-expander
    ((client client) (environment environment))
  (trucler:quasi-clone environment
    :variable-description
    (remove-if #'unavailable-under-restriction-p
               (variable-description environment))
    :function-description
    (remove-if #'unavailable-under-restriction-p
               (function-description environment))
    :block-description
    (remove-if #'unavailable-under-restriction-p
               (block-description environment))
    :tag-description
    (remove-if #'unavailable-under-restriction-p
               (tag-description environment))
    :unrestricted-environment environment))
