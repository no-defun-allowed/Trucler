(cl:in-package #:trucler-reference)

(defgeneric unavailable-under-restriction-p (description))

(defmethod unavailable-under-restriction-p (description)
  nil)

(defgeneric restricted-description-type (description))

(macrolet ((unavailable-under-restriction (&rest names)
             `(progn
                ,@(loop for (name type) in names
                        collect `(progn
                                   (defmethod unavailable-under-restriction-p
                                       ((description ,name))
                                     t)
                                   (defmethod restricted-description-type
                                       ((description ,name))
                                     ,type))))))
  (unavailable-under-restriction
   (trucler:lexical-variable-description "lexical variable")
   (trucler:special-variable-description "special variable")
   (trucler:local-function-description "local function")
   (trucler:block-description "block")
   (trucler:tag-description "tag")))

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
