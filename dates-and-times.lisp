;;;; dates-and-times.lisp

(in-package #:dates-and-times)

(defparameter *days* '(Monday Tuesday Wednesday Thursday Friday Saturday Sunday))
(defparameter *months* '((January . 31) (February . 28) (March . 31) (April . 30)
                         (May . 31) (June . 30) (July . 31) (August . 31)
                         (September . 30) (October . 31) (November . 30)
                         (December . 31)))

(defclass timestamp ()
  ((sec :initarg :sec :accessor sec)
   (minute :initarg :minute :accessor minute)
   (hour :initarg :hour :accessor hour)
   (day :initarg :day :accessor day)
   (month :initarg :month :accessor month)
   (year :initarg :year :accessor year)
   (day-of-week :initarg :day-of-week :accessor day-of-week)
   (daylight-savings-p :initarg :daylight-savings-p :accessor daylight-savings-p)
   (timezone :initarg :timezone :accessor timezone)))

(defmethod universal->timestamp ((universal integer))
  (multiple-value-bind (sec minute hour day month year day-of-week
                              daylight-savings-p timezone)
      (decode-universal-time universal)
    (make-instance 'timestamp :sec sec :minute minute :hour hour :day day
                   :month month :year year :day-of-week day-of-week
                   :daylight-savings-p daylight-savings-p :timezone timezone)))

(defmethod timestamp->universal ((ts timestamp))
  (encode-universal-time (sec ts) (minute ts) (hour ts) (day ts)
                         (month ts) (year ts) (timezone ts)))

(defmethod epoch->universal ((universal integer))
  (let ((seconds-in-a-year 31556926))
    (+ universal (* seconds-in-a-year 70))))

(defmethod timestamp->epoch ((ts timestamp))
  (universal->epoch (timestamp->universal ts)))

(defmethod universal->epoch ((epoch integer))
  (let ((seconds-in-a-year 31556926))
    (- epoch (* seconds-in-a-year 70))))

(defmethod epoch->timestamp ((epoch integer))
  (universal->timestamp (epoch->universal epoch)))

(defmethod leap-year-p ((ts timestamp))
  (if (= (mod (year ts) 4) 0)
      t))

(defmacro compare-timestamps (timestamp1 timestamp2 function &key (return t)
                                                               value-functions)
  `(cond ,@(mapcar #'(lambda (fn) `((funcall ,function (funcall #',fn ,timestamp1)
                                             (funcall #',fn ,timestamp2)) ,return))
                   value-functions)))

(defmacro define-timestamp-comparison (name function &key (return t)
                                                       (value-functions '(year
                                                                          month
                                                                          day)))
  `(defmethod ,name ((ts1 timestamp) (ts2 timestamp))
     (compare-dates ts1 ts2 ,function :return ,return
                    :value-functions ,value-functions)))

(define-timestamp-comparison date< #'<)
(define-timestamp-comparison date> #'>)
(define-timestamp-comparison date= #'=)
(define-timestamp-comparison date<= #'<=)
(define-timestamp-comparison date>= #'>=)

(define-timestamp-comparison time< #'< :value-functions (hour minute second))
(define-timestamp-comparison time> #'> :value-functions (hour minute second))
(define-timestamp-comparison time= #'= :value-functions (hour minute second))
(define-timestamp-comparison time<= #'<= :value-functions (hour minute second))
(define-timestamp-comparison time>= #'>= :value-functions (hour minute second))
