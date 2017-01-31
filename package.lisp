;;;; package.lisp

(defpackage #:dates-and-times
  (:use #:cl #:peyton-utils)
  (:export #:*days*
           #:*months*
           #:timestamp
           #:sec
           #:minute
           #:hour
           #:day
           #:month
           #:year
           #:day-of-week
           #:daylight-savings-p
           #:timezone
           #:universal->timestamp
           #:timestamp->universal
           #:universal->epoch
           #:timestamp->epoch
           #:epoch->universal
           #:epoch->timestamp
           #:leap-year-p
           #:define-timestamp-comparison
           #:date<
           #:date<=
           #:date>
           #:date>=
           #:date=
           #:time<
           #:time<=
           #:time>
           #:time>=
           #:time=))
