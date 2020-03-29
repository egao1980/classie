#|
This file is a part of (C)LASS(IE)
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
(c) 2020 Nikolai Matiushev (egao1980@gmail.com)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defclass classie-file (asdf:source-file)
  ((output :initarg :output :initform NIL :accessor output))
  (:default-initargs :type "classie")
  (:documentation "An ASDF source-file component to allow compilation of LASS to CSS in ASDF systems using classie extentions."))

;; Hack to ensure that ASDF recognises the class
;; as a keyword, which I think is currently a bug.
;; If LASS is only in DEFSYSTEM-DEPENDS-ON and the
;; system tries to specify a LASS-FILE component,
;; ASDF complains about an unknown component type
;; even though the class exists. Loading LASS and
;; the system separately however works just fine.
;;
;; Since ASDF by default searches classes in
;; ASDF/INTERFACE we simply smuggle our own class
;; into that package. Sneaky, but the only sensible
;; workaround for now.
(defclass asdf/interface::classie-file (classie-file)
  ())

(defmethod asdf:source-file-type ((c classie-file) (s asdf:module)) "lass")

(defmethod asdf:output-files ((op asdf:compile-op) (c classie-file))
  (values
   (list (merge-pathnames
          (or (output c)
              (pathname-name (asdf:component-pathname c)))
          (make-pathname :type "css" :defaults (asdf:component-pathname c))))
   T))

(defmethod asdf:perform ((op asdf:load-op) (c classie-file))
  T)

(defmethod asdf:perform ((op asdf:compile-op) (c classie-file))
  (let* ((pathname (asdf:component-pathname c))
         (*default-pathname-defaults* (cl-fad:pathname-directory-pathname pathname)))
    (lass:generate pathname
                   :out (first (asdf::output-files op c)))))
