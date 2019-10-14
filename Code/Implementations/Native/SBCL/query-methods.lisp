(cl:in-package #:trucler-native-sbcl)

(defun leaf-type (leaf env default)
  (sb-kernel:type-specifier
   (sb-kernel:type-intersection
    (sb-c::leaf-type leaf)
    (let ((entry (assoc leaf (sb-c::lexenv-type-restrictions env))))
      (if entry
          (cdr entry)
          default)))))

(defun var-type (var env)
  (leaf-type var env sb-kernel:*universal-type*))

(defun fun-type (var env)
  (leaf-type var env sb-kernel:*universal-fun-type*))

(defun leaf-dynamic-extent (leaf env)
  (declare (ignore env))
  (and (sb-c::leaf-dynamic-extent leaf) t))

(defmethod trucler:describe-variable
    ((client client) (env sb-kernel:lexenv) (name symbol))
  (let ((entry (assoc name (sb-c::lexenv-vars env))))
    (if (not entry)
        (ecase (sb-int:info :variable :kind name)
          (:special
           (make-instance 'global-special-variable-description
             :name name))
          (:macro
           (make-instance 'global-symbol-macro-description
             :name name
             :expansion (sb-int:info :variable :macro-expansion name)))
          (:constant
           (make-instance 'constant-variable-description
             :name name
             :value (symbol-value name)))
          (:unknown nil))
        (let ((var (cdr entry)))
          (etypecase var
            (sb-c::lambda-var
             (make-instance 'lexical-variable-description
               :name name
               :identity var
               :dynamic-extent (leaf-dynamic-extent var env)
               :ignore (cond ((sb-c::lambda-var-ignorep var)
                              'cl:ignore)
                             ((sb-c::leaf-ever-used var)
                              'cl:ignorable)
                             (t nil))
               :type (var-type var env)))
            (sb-c::global-var
             (ecase (sb-c::global-var-kind var)
               (:special
                (make-instance 'local-special-variable-description
                  :name name
                  :type (var-type var env)))
               (:global
                (make-instance 'global-variable-description
                  :name name
                  :type (var-type var env)))
               (:unknown nil)))
            (sb-c::constant
             (make-instance 'constant-variable-description
               :name name
               :value (sb-c::constant-value var)))
            (cons
             (make-instance 'local-symbol-macro-description
               :name name
               :expansion (cdr var))))))))

(defmethod trucler:describe-function
    ((client client) (env sb-kernel:lexenv) name)
  (let ((entry (assoc name (sb-c::lexenv-funs env) :test #'equal)))
    (if (not entry)
        (case (sb-int:info :function :kind name)
          (:macro
           (make-instance 'global-macro-description
             :name name
             :expander (sb-int:info :function :macro-function name)))
          (:special-form
           (make-instance 'special-operator-description
             :name name))
          (:function
           (make-instance 'global-function-description
             :name name
             :type (if (eq (sb-int:info :function :where-from name) :declared)
                       (sb-int:proclaimed-ftype name)
                       sb-kernel:*universal-fun-type*)
             :inline (sb-int:info :function :inlinep name))))
        (let ((fun (cdr entry)))
          (etypecase fun
            (sb-c::functional
             (make-instance 'local-function-description
               :name name
               :type (fun-type fun env)
               :identity fun
               :inline (sb-c::functional-inlinep fun)
               :dynamic-extent (leaf-dynamic-extent fun env)))
            (sb-c::defined-fun
             (make-instance 'global-function-description
               :name name
               :type (fun-type fun env)
               :inline (sb-c::defined-fun-inlinep fun)
               :dynamic-extent (leaf-dynamic-extent fun env)))
            (cons
             (make-instance 'local-macro-description
               :name name
               :expander (cdr fun))))))))

(defmethod trucler:describe-block
    ((client client) (env sb-kernel:lexenv) (name symbol))
  (let ((entry (assoc name (sb-c::lexenv-blocks env))))
    (if entry
        (make-instance 'trucler:block-description
          :name name
          :identity (car entry))
        nil)))

(defmethod trucler:describe-tag
    ((client client) (env sb-kernel:lexenv) tag)
  (let ((entry (assoc tag (sb-c::lexenv-tags env))))
    (if entry
        (make-instance 'tag-description
          :name tag
          :identity (cdr entry))
        nil)))

(defmethod trucler:describe-optimize
    ((client client) (env sb-kernel:lexenv))
  (let ((policy (or (sb-c::lexenv-policy env)
                    sb-c::*policy*)))
    (make-instance 'optimize-description
      :speed (sb-c::policy-quality policy 'speed)
      :compilation-speed (sb-c::policy-quality policy 'compilation-speed)
      :debug (sb-c::policy-quality policy 'debug)
      :space (sb-c::policy-quality policy 'space)
      :safety (sb-c::policy-quality policy 'safety))))

(defmethod trucler:global-environment
    ((client client) (env sb-kernel:lexenv))
  (sb-kernel:make-null-lexenv))

(defmethod trucler:global-environment-p
    ((client client) (env sb-kernel:lexenv))
  (sb-c::null-lexenv-p env))
