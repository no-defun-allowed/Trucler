(cl:in-package #:asdf-user)

(defsystem "trucler-reference"
  :depends-on ("trucler-base")
  :serial t
  :components
  ((:file "packages")
   (:file "client")
   (:file "environment")
   (:file "restrict-for-macrolet-expander")
   (:file "query-methods")
   (:file "low-level-augmentation-methods")
   (:file "augmentation-methods")
   (:file "macro-methods")))
