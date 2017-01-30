;;;; dates-and-times.lisp

(in-package #:dates-and-times)

(defparameter *days* '(Monday Tuesday Wednesday Thursday Friday Saturday Sunday))
(defparameter *months* '((January . 31) (February . 28) (March . 31) (April . 30)
                         (May . 31) (June . 30) (July . 31) (August . 31)
                         (September . 30) (October . 31) (November . 30)
                         (December . 31)))
(defclass date ()
  ((sec :initarg :sec :accessor sec)
   (minute :initarg :minute :accessor minute)
   (hour :initarg :hour :accessor hour)
   (day :initarg :day :accessor day)
   (month :initarg :month :accessor month)
   (year :initarg :year :accessor year)
   (day-of-week :initarg :day-of-week :accessor day-of-week)
   (daylight-savings-p :initarg :daylight-savings-p :accessor daylight-savings-p)
   (timezone :initarg :timezone :accessor timezone)))

(defmethod universal->date ((universal integer))
  (multiple-value-bind (sec minute hour day month year day-of-week
                              daylight-savings-p timezone)
      (decode-universal-time universal)
    (make-instance 'date :sec sec :minute minute :hour hour :day day
                   :month month :year year :day-of-week day-of-week
                   :daylight-savings-p daylight-savings-p :timezone timezone)))

(defmethod leap-year-p ((date date))
  (if (= (mod (year date) 4) 0)
      t))

(defmacro compare-dates (date1 date2 function &key (return t) value-functions)
  `(cond ,@(mapcar #'(lambda (fn) `((funcall ,function (funcall #',fn ,date1)
                                             (funcall #',fn ,date2)) ,return))
                   value-functions)))

(defmacro define-date-comparison (name function &key (return t) (value-functions
                                                                 '(year month day)))
  `(defmethod ,name ((d1 date) (d2 date))
     (compare-dates d1 d2 ,function :return ,return
                    :value-functions ,value-functions)))

(define-date-comparison date< #'<)
(define-date-comparison date> #'>)
(define-date-comparison date= #'=)
(define-date-comparison date<= #'<=)
(define-date-comparison date>= #'>=)
