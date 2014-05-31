
;;(require :iolib)
;;(require :iolib.termios)
(require :cl-ppcre)
(require :drakma)
(require :local-time)

;;  (setf ttypath (make-pathname :name "/dev/ttyUSB0"))
;;  (setf mkiii (open "/dev/ttyUSB0" :direction :io
;;		    :if-exists :overwrite
;;		    :sharing :private
;;		    :basic t))

;;(defstruct wxpoint (:print-function print-wxpoint)
(defstruct wxpoint
  date
  time
  temperature
  humidity
  barometer
  wind-direction
  wind-speed
  gust
  rain
  battery
  checksum
  dewpoint
  wind-chill
  rain-last-hour)

;; set up stream for output
;;(defun print-wxpoint (wxpoint stream depth)
(defun print-wxpoint (wxp)
  "Print out the current weather reading"
  (format t " Temperature:  ~4d F      Humidity: ~d%~%"
	  (wxpoint-temperature wxp)
	  (wxpoint-humidity wxp))
  (format t "   Barometer: ~,2f (in Hg)    Wind: ~d@~d~%"
	  (wxpoint-barometer wxp)
	  (wxpoint-wind-speed wxp)
	  (wxpoint-wind-direction wxp))
  (format t "    Dewpoint:  ~,1f F          Gust: ~d~%"
	  (wxpoint-dewpoint wxp)
	  (wxpoint-gust wxp))
  (format t " Rain (hour):  ~,2f (in)  Windchill: ~d F~%"
	  (wxpoint-rain-last-hour wxp)
	  (wxpoint-wind-chill wxp))
  (format t "Rain (today):  ~,2f (in)    Battery: ~,2f V~2%"
	  (wxpoint-rain wxp)
	  (wxpoint-battery wxp)))

(defun  calc-wind-chill (tempf wspeed)
  "Calculate wind chill index using temperature (Fahrenheit) and wind speed (MPH)"
  ;; JAG/TI Nov 2001 US Wind Chill Index formula - https://en.wikipedia.org/wiki/Wind_chill
  ;; NOTE - valid only when temperature <= 50 F and wind speed > 4.0 MPH
  (if (and (<= tempf 50) (> wspeed 4))
      (+ 35.74 (* 0.6215 tempf)
	 (* 0.4275 tempf (expt wspeed 0.16))
	 (- 0 (* 35.75 (expt wspeed 0.16))))
      tempf))

(defun calc-dewpoint (tempc humidity)
  "Calculate dewpoint via the Magnus formula using temperature (Celsius) and relative humidity (%)"
  (let* ((b 17.67)
	(c 243.5)
	(gamma (+ (log (/ humidity 100.0))
		  (/ (* b tempc) (+ c tempc)))))
    (/ (* c gamma) (- b gamma))))

(defun fahrenheit-to-celsius (tempf)
  "Convert temperature Fahrenheit to Celsius"
  (/ (- tempf 32.0) 1.8))

(defun celsius-to-fahrenheit (tempc)
  "Convert temperature Celsius to Fahrenheit"
  (+ (* tempc 1.8) 32.0))			      

(defun parse-float (string)
  "Return a float read from string, and the index to the remainder of string."
  (multiple-value-bind (integer i)
      (parse-integer string :junk-allowed t)
    (multiple-value-bind (fraction j)
        (parse-integer string :start (+ i 1) :junk-allowed t)
      (values (float (+ integer (/ fraction (expt 10 (- j i 1))))) j))))

(defun calc-checksum (string)
  "Return integer checksum of string."
  (let ((sum 0))
    (map nil #'(lambda (char) (setf sum (+ sum (char-code char)))) string)
    (mod sum 256)))

(defun upload-to-wunderground (wxp)
  "Upload the current weather stats to wunderground.com."
  (ignore-errors 
      (drakma:http-request (format nil "http://rtupdate.wunderground.com/weatherstation/updateweatherstation.php?ID=xxxx&PASSWORD=xxxx&dateutc=~a&winddir=~d&windspeedmph=~d&windgustmph=~d&tempf=~d&dailyrainin=~f&rainin=~f&baromin=~f&dewptf=~,1f&humidity=~d&softwaretype=wxlispbeta&action=updateraw&realtime=1&rtfreq=4"
				   (drakma:url-encode (timestamp-UTC) :utf-8)
				   (wxpoint-wind-direction wxp)
				   (wxpoint-wind-speed wxp)
				   (wxpoint-gust wxp)
				   (wxpoint-temperature wxp)
				   (wxpoint-rain wxp)
				   (wxpoint-rain-last-hour wxp)
				   (wxpoint-barometer wxp)
				   (wxpoint-dewpoint wxp)
				   (wxpoint-humidity wxp))
			   :connection-timeout 15)))

(defun timestamp-UTC ()
  "return current time UTC in YYYY-MM-DD HH:MM:SS format"
  (let ((UTCtime (local-time:timestamp+ (local-time:now) 8 :hour)))
    (format nil "~4d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
	    (local-time:timestamp-year UTCtime)
	    (local-time:timestamp-month UTCtime)
	    (local-time:timestamp-day UTCtime)
	    (local-time:timestamp-hour UTCtime)
	    (local-time:timestamp-minute UTCtime)
	    (local-time:timestamp-second UTCtime))))

(defun rain-last-hour (amt)
  "Return the amount of rain for the last hour."
  (setf current (local-time:now))
  (setf hourago (local-time:timestamp- current 3600 :seconds))
  ;; iteratively
  (let ((newlist '()))
    (dolist (x *rainlist*)
      (if (local-time:timestamp> (cdar x) hourago)
	(append x newlist)))
    (setf *rainlist* (append newlist)))
  )

(defun timelistmaint-helper (x hourago)
  "Return list of timestamps aged an hour or less."
  (if (null x) '()
      (if (local-time:timestamp> (caar x) hourago)
	  x
	  (timelistmaint-helper (cdr x) hourago))))

(defun timelistmaint (rainamt timelist)
  "Return an alist of current rain timestamps with the latest appended."
  (if (zerop rainamt)
      (timelistmaint-helper timelist (local-time:timestamp- (local-time:now) 3600 :sec))
      (append (timelistmaint-helper timelist (local-time:timestamp- (local-time:now) 3600 :sec))
	  (list (cons (local-time:now) rainamt)))))

(defun add-em-all (rainlist)
  "Sum the rainfall amounts in the rainlist."
  (if (null rainlist)
      0.0
      (+ (cdar rainlist) (add-em-all (cdr rainlist)))))

;;(defun wxlisp (bleh)
;;  "Main loop"
;;  (setf ttypath (make-pathname :directory '(:absolute "dev") :name "ttyUSB0"))
;;  (setf mkiii (open "/dev/ttyUSB0" :direction :io
;;		    :if-exists :overwrite
;;		    :external-format :ascii))
;;(itty:stty mkiii :b9600 :raw t)


  ;;>H,DATE,TIME,TEMP,HUM,BARO,WDIR,WSPD,WSHI,RF_DAY,BATT,CHILL
  ;;D,12/07,10:25, 43, 93,29.70,067, 3, 10, 1.32,6.51, 42,!197
  ;;E  NO DATA RECEIVED

;;(setf drakma:*header-stream* *standard-output*)
(defun wx ()
  "Read Rainwise Mk III weather station, upload to wunderground.com. Repeat."

  (setf wxp (make-wxpoint))
  (defparameter *gusts* (make-array 150 :initial-element 0))
  (defparameter *gustptr* 0)
  (defparameter *rainlist* '())
  (defparameter *lastrain* 0.0)
  (setf *upload-success-count* 0)
  (setf *upload-failure-count* 0)
  (setf *upload-last-failure-time* (local-time:now))
  (setf *station-read-success-count* 0)
  (setf *station-read-failure-count* 0)
  (setf *station-read-last-failure-time* (local-time:now))
  (with-open-file (mkiii "/dev/ttyUSB0"
			 :direction :io
			 :if-exists :overwrite
			 :external-format :ascii)
    (do (( i 0 (+ i 1)))
	(nil 'done)
      (setf rawstring (read-line mkiii))
      (setf ckstr (subseq rawstring 0 (position #\! rawstring)))
      ;; (format t "pos: ~d WX: ~a~%" (position #\! rawstring) ckstr)
      (setf rawstring (remove #\space (cl-ppcre:split "," rawstring)))
      (if (string/= "E" (car rawstring))
	  (progn
	    (setf *station-read-success-count* (+ 1 *station-read-success-count*))
	    ;; assign strings to wxp
	    (setf tag (nth 0 rawstring))
	    (setf (wxpoint-date wxp) (nth 1 rawstring))
	    (setf (wxpoint-time wxp) (nth 2 rawstring))
	    (setf (wxpoint-temperature wxp) (parse-integer (nth 3 rawstring)))
	    (setf (wxpoint-humidity wxp) (parse-integer (nth 4 rawstring)))
	    (setf (wxpoint-barometer wxp) (parse-float (nth 5 rawstring)))
	    (setf (wxpoint-wind-direction wxp) (parse-integer (nth 6 rawstring)))
	    (setf (wxpoint-wind-speed wxp) (parse-integer (nth 7 rawstring)))
	    (setf (wxpoint-gust wxp) (parse-integer (nth 8 rawstring)))
	    (setf (wxpoint-rain wxp) (parse-float (nth 9 rawstring)))
	    (setf (wxpoint-battery wxp) (parse-float (nth 10 rawstring)))
	    (setf (wxpoint-wind-chill wxp) (parse-integer (nth 11 rawstring)))
	    (setf (wxpoint-checksum wxp) (parse-integer (subseq (nth 12 rawstring) 1 4)))
	    ;; calculate dewpoint
	    (if (zerop (wxpoint-humidity wxp))
		(setf (wxpoint-humidity wxp) 1)) ; prevent divide-by-zero exception in calc-dewpoint log fn
	    (setf (wxpoint-dewpoint wxp) (celsius-to-fahrenheit
					  (calc-dewpoint
					   (fahrenheit-to-celsius (wxpoint-temperature wxp))
					   (wxpoint-humidity wxp))))
	    ;; calculate wind-chill factor
	    ;;(setf (wxpoint-wind-chill wxp) (calc-wind-chill (wxpoint-temperature wxp) (wxpoint-wind-speed wxp)))
	    ;; total rain
	    ;;(if (< *lastrain* (wxpoint-rain wxp)) ; we have rain 
	    (setf *rainlist* (timelistmaint (- (wxpoint-rain wxp) *lastrain*) *rainlist*))
	    (setf *lastrain* (wxpoint-rain wxp)) ; else no rain or numbers reset at midnight
	    (setf (wxpoint-rain-last-hour wxp) (add-em-all *rainlist*))							  
		    
	    ;; wind gust - last five minutes
	    (setf (svref *gusts* *gustptr*) (wxpoint-wind-speed wxp))
	    (setf *gustptr* (mod (+ 1 *gustptr*) 150))
	    (setf (wxpoint-gust wxp) (reduce #'max *gusts*))
	    ;; print wxp
	    (print-wxpoint wxp)
	
	    (if (evenp i)
		(if (search "success" (upload-to-wunderground wxp))
		    (setf *upload-success-count* (+ 1 *upload-success-count*))
		    (progn
		      (setf *upload-failure-count* (+ 1 *upload-failure-count*))
		      (setf *upload-last-failure-time* (local-time:now))))))
	  (progn
	    (setf *station-read-failure-count* (+ 1 *station-read-failure-count*))
	    (setf *station-read-failure-time* (local-time:now))))
      (format t "WX station read successes: ~d   failures: ~d"
	      *station-read-success-count* *station-read-failure-count*)
      (if (not (zerop *station-read-failure-count*))
	  (progn
	    (format t "   Time of last failure: ")
	    (local-time:format-timestring t *station-read-failure-time* :format local-time:+asctime-format+)))
      (terpri)
      (format t "         Upload successes: ~d   failures: ~d"
	      *upload-success-count* *upload-failure-count*)
      (if (not (zerop *upload-failure-count*))
	  (progn
	    (format t "   Time of last failure: ")
	    (local-time:format-timestring t *upload-last-failure-time* :format local-time:+asctime-format+)))
      (format t "~%--------------------------------------~%"))))
	      

;; - todo
;;--- TODO: daylight savings affecting UTC calculation
;;--- TODO: better wind gust calculation - DONE, kinda
;;--- TODO: counters for upload success/failure and station read success/failure - DONE
;;--- TODO: crc-16 calculation
;;--- TODO: 2m/10m averages
;;--- TODO: total rain - DONE
;;--- TODO: communication protocol with station - set to known state
;;--- TODO: standalone
;;--- TODO: divide by zero in log function? - workaround in place. Need to replace temp/humidity sensor. - DONE
;;--- TODO: unwind-protect around station read?
;;--- TODO: rain-last-hour not clearing after an hour.
