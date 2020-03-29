(defpackage lassie
  (:use :cl
        :parse-float
        :physical-quantities))

(in-package :lassie)

(defun adjust-luminosity (color amount)
  (let* ((rgb (cl-colors:parse-hex-rgb color))
         (r (cl-colors:rgb-red rgb))
         (g (cl-colors:rgb-green rgb))
         (b (cl-colors:rgb-blue rgb)))
    (multiple-value-call (lambda (r g b) (cl-colors:print-hex-rgb (cl-colors:rgb r g b)))
      (multiple-value-call #'dufy:hsl-to-rgb
        (multiple-value-call (lambda (h s l) (values h s (+ l amount)))
          (dufy:rgb-to-hsl r g b))))))

(defmacro darken (color amount)
  `(adjust-luminosity ,color (- ,amount)))

(defmacro lighten (color amount)
  `(adjust-luminosity ,color ,amount))

(defun adjust-opacity (color amount)
  (multiple-value-bind (rgb alpha) (cl-colors:parse-hex-rgb color)
    (multiple-value-call
        (lambda (r g b)
          (format
           nil
           "rgba(~$, ~$, ~$, ~$)"
           r g b
           (max 0.0
                (min 1.0
                     (+ (if alpha alpha 1.0) amount)))))
      (dufy:rgb-to-qrgb
       (cl-colors:rgb-red rgb)
       (cl-colors:rgb-green rgb)
       (cl-colors:rgb-blue rgb)))))


(defmacro fade-out (color amount)
  `(adjust-opacity ,color (- ,amount)))

(defmacro fade-in (color amount)
  `(adjust-opacity ,color ,amount))


(defmacro weighted-sum (a b w)
  `(min 1.0 (+ (* ,w ,a) (* (- 1.0 ,w) ,b))))

(defun mix-colors (color-a color-b &key (weight 0.5))
  (let ((a (cl-colors:parse-hex-rgb color-a))
        (b (cl-colors:parse-hex-rgb color-b)))
    (cl-colors:print-hex-rgb
     (cl-colors:rgb
      (weighted-sum
       (cl-colors:rgb-red a)
       (cl-colors:rgb-red b)
       weight)
      (weighted-sum
       (cl-colors:rgb-green a)
       (cl-colors:rgb-green b)
       weight)
      (weighted-sum
       (cl-colors:rgb-blue a)
       (cl-colors:rgb-blue b)
       weight)))))


(handler-case
    (progn
      (define-si-units)
      (define-unit |%| :def (0.01) :alias (|percent| |percents|) :prefix-test (constantly nil))
      (define-unit |in| :def (0.0254 |m|) :alias (|inch| |inches|) :prefix-test (constantly nil))
      (define-unit |pt| :def (1/72 |inch|) :alias (|point| |points|) :prefix-test (constantly nil))
      (define-unit |px| :def (1/96 |inch|) :alias (|pixel| |pixels|) :prefix-test (constantly nil)))
  (unit-definition-conflict-error (e)
    (warn "Attempting to redefine units definitions.")))


(defun parse-css-number (x &key unit)
  (cond ((stringp x)
         (multiple-value-bind (val idx) (parse-float x :junk-allowed t)
           (if unit
               (make-quantity :value val :unit (make-unit (list unit 1)))
               (if (>= idx (length x))
                   val
                   (make-quantity :value val :unit (make-unit (list (subseq x idx) 1)))
                   ))))
        ((symbolp x)
         (parse-css-number (string x) :unit unit))
        (unit
         (make-quantity :value x :unit (make-unit (list unit 1))))
        (t
         x)))

(defun reduce-percents (q)
  (if (quantityp q)
      (let ((units (unit q))
            (value (value q)))
        (if (not (alexandria:emptyp units))
            (multiple-value-bind (percents others)
                (loop for u in units
                      if (string= "%" (uf-unit u))
                      collect u into percents
                      else collect u into others
                      finally (return (values percents others)))
              (cond ((alexandria:emptyp percents)
                     q)
                    ((alexandria:emptyp others)
                     (let ((power (- (uf-power (car percents)) 1)))
                       (make-quantity :value (/ value (expt 100.0 power))
                                      :unit (make-unit (list "%" (max 1 power))))))
                    (t (make-quantity :value (/ value (expt 100.0 (uf-power (car percents))))
                                      :unit others))))
            q))
      q))

(defun resolve-css-number (x)
  "Returns CSS number with unit, only power 1 units are properly supported."
  (if (quantityp x)
      (let* ((q (reduce-percents x))
             (value (value q))
             (units (unit q))
             (unit (if (alexandria:emptyp units)
                       nil
                       (uf-unit (car units)))))
        (values (if (string= "%" unit) (/ value 100.0) value) unit value))
      (values x nil x)))

(defun css-number-string-format (value unit raw)
  (cond ((string= "%" unit)
         (format nil "~$%" raw))
        (unit
         (format nil "~$~A" value unit))
        ((integerp value)
         (format nil "~A" value))
        (t
         (format nil "~$" value))))

(defun css-number-string (q)
  (multiple-value-call 'css-number-string-format (resolve-css-number q)))



(defun wrap-funcall (fn q)
  (multiple-value-bind (value unit raw) (resolve-css-number q)
    (declare (ignore raw))
    (let ((result (funcall fn value)))
      (values result unit (if (string= "%" unit) (* 100.0 result) result)))))

(defun wrap-funcall-raw (fn q)
  (multiple-value-bind (value unit raw) (resolve-css-number q)
    (declare (ignore value))
    (let ((result (funcall fn raw)))
      (values (if (string= "%" unit) (/ result 100.0) result) unit result))))


(defun resolve-css-arg (expr)
  (resolve-css-number
   (parse-css-number
    (lass:resolve expr))))

(lass:define-property-function mix (a b) (mix-colors (lass:resolve a) (lass:resolve b)))
(lass:define-property-function fade-in (color amount) (fade-in (lass:resolve color) (resolve-css-arg amount)))
(lass:define-property-function fade-out (color amount) (fade-out (lass:resolve color) (resolve-css-arg amount)))
(lass:define-property-function darken (color amount) (darken (lass:resolve color) (resolve-css-arg amount)))
(lass:define-property-function lighten (color amount) (lighten (lass:resolve color) (resolve-css-arg amount)))


(defmacro define-css-op (op)
  `(lass:define-property-function ,op (a b)
     (css-number-string
      (,(intern (string-upcase (concatenate 'string "q" op)))
       (parse-css-number (lass:resolve a))
       (parse-css-number (lass:resolve b))))))

(defmacro define-css-fun (op)
  `(lass:define-property-function ,op (a)
     (css-number-string
      (,(intern (string-upcase (concatenate 'string "q" op)))
       (parse-css-number (lass:resolve a))))))

(define-css-op "+")
(define-css-op "-")
(define-css-op "/")
(define-css-op "*")

(lass:define-property-function ceil (a)
  (multiple-value-call 'css-number-string-format
    (wrap-funcall-raw #'ceiling (parse-css-number (lass:resolve a)))))

(lass:define-property-function ceiling (a)
  (multiple-value-call 'css-number-string-format
    (wrap-funcall #'ceiling (parse-css-number (lass:resolve a)))))

(defmacro bind-vars* (bindings &body body)
  `(let ((lass:*vars* (let ((table (make-hash-table)))
                        (maphash #'(lambda (k v) (setf (gethash k table) v)) lass:*vars*)
                        table)))
     (progn 
       (loop for (k v) in ,bindings
             do (setf (gethash k lass:*vars*)
                      (lass:resolve v)))
       ,@body)))



(lass:define-special-block include (file)
  (let* ((eof (gensym "EOF"))
         (in (lass:resolve file))
         (path (cl-fad:merge-pathnames-as-file *default-pathname-defaults* in))
         (*default-pathname-defaults* (cl-fad:merge-pathnames-as-directory *default-pathname-defaults* file)))
    (bind-vars* '()
      (apply #'lass:compile-sheet
             (with-open-file (instream path :direction :input)
               (loop for read = (read instream NIL eof)
                     until (eql read eof)
                     collect read))))))

(lass:define-special-block let* (bindings &rest body)
  (bind-vars* bindings
    (apply #'lass:compile-sheet body)))

(setf (lass:property-function "values")
      (lambda (&rest args) (format nil "~{~a~^ ~}" (mapcar #'lass:resolve args))))

