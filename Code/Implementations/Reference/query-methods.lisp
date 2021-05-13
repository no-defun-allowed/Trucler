(cl:in-package #:trucler-reference)

(defmethod trucler:describe-variable ((client client) (environment environment) name)
  (let* ((descriptions (variable-description environment))
         (description (find name descriptions :test #'eq :key #'trucler:name)))
    (when (null description)
      (let* ((global-environment (global-environment environment))
             (global-description (trucler:describe-variable client global-environment name)))
        (setf description global-description)
        (unless (null global-description)
          ;; Cache the global description locally.
          (reinitialize-instance environment
            :variable-description (cons description descriptions)))))
    description))

(defmethod trucler:describe-function ((client client) (environment environment) name)
  (let* ((descriptions (function-description environment))
         (description (find name descriptions :test #'equal :key #'trucler:name)))
    (when (null description)
      (let* ((global-environment (global-environment environment))
             (global-description (trucler:describe-function client global-environment name)))
        (setf description global-description)
        (unless (null global-description)
          ;; Cache the global description locally.
          (reinitialize-instance environment
            :function-description (cons description descriptions)))))
    description))

(defmethod trucler:describe-block ((client client) (environment environment) name)
  (let* ((descriptions (block-description environment))
         (description (find name descriptions :test #'eq :key #'trucler:name)))
    description))

(defmethod trucler:describe-tag ((client client) (environment environment) tag)
  (let* ((descriptions (tag-description environment))
         (description (find tag descriptions :test #'eql :key #'trucler:name)))
    description))

(defmethod trucler:describe-optimize ((client client) (environment environment))
  (let* ((description (optimize-description environment)))
    (when (null description)
      (let* ((global-environment (global-environment environment))
             (global-description (trucler:describe-optimize client global-environment)))
        (setf description global-description)
        (unless (null global-description)
          ;; Cache the global description locally.
          (reinitialize-instance environment
            :optimize-description description))))
    description))

(defmethod trucler:describe-declarations ((client client)
                                          (environment environment))
  (let ((description (declaration-description environment)))
    (if (null description)
        ;; Get from the global environment.
        (trucler:describe-declarations client (global-environment environment))
        description)))

(defmethod trucler:global-environment
    ((client client) (environment environment))
  (global-environment environment))

(defmethod trucler:global-environment-p
    ((client client) (environment environment))
  (and (null (function-description environment))
       (null (variable-description environment))
       (null (block-description environment))
       (null (tag-description environment))
       (null (optimize-description environment))))
