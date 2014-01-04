;;; geo-ip.el --- Query DuckDuckGo for external IP, latitude,
;;; longitude and location.

;;; Commentary:

;; Several functions for determining location based on external IP
;; address.  `geo-ip-lat-lon-loc-ip' should be the only externally
;; useable function as the rest are simple regular expression based
;; text extraction helper functions.

;;; Code:

(require 'url)


(defun geo-ip-location-name ()
  "Extract and return the location name string."
  (save-excursion
    (let ((beginning (search-forward-regexp "mapquest\\.com/\\?q=[0-9.,-]+\">"))
          (end (1- (search-forward-regexp "<"))))
      (buffer-substring-no-properties beginning end))))

(defun geo-ip-latitude-longitude ()
  "Extract and return the numeric latitude/longitude pair."
  (save-excursion
    (let ((beginning (search-forward-regexp "mapquest\\.com/\\?q="))
          (end (1- (search-forward-regexp "\""))))
      (mapcar
       #'(lambda (x) (string-to-number x))
       (split-string (buffer-substring-no-properties beginning end) ",")))))

(defun geo-ip-ip-address ()
  "Extract and return the IP address string."
  (save-excursion
    (let ((beginning
           (search-forward-regexp
            "Your[[:space:]]+IP[[:space:]]+address[[:space:]]+is[[:space:]]+"))
          (end (1- (search-forward-regexp " "))))
      (buffer-substring-no-properties beginning end))))

(defun geo-ip-lat-lon-loc-ip (callback)
  "Make a request to DuckDuckGo for the external IP address of this machine.

Extract latitude, longitude, location name and IP address from
the response and pass them as arguments to CALLBACK."
  (url-retrieve
   "http://duckduckgo.com/?q=ip"
   #'(lambda (status callback)
       (unwind-protect
           (let* ((lat-lon (geo-ip-latitude-longitude))
                  (lat (car lat-lon))
                  (lon (cadr lat-lon))
                  (loc (geo-ip-location-name))
                  (ip (geo-ip-ip-address)))
             (funcall callback lat lon loc ip))
         (kill-buffer)))
   `(,callback)))

(defun geo-ip--set-calendar-vars (latitude longitude name ip)
  "Set calendar location variables."
  (setq calendar-latitude latitude
        calendar-longitude longitude
        calendar-location-name name))

(defun geo-ip-update ()
  "Update variables which require geolocation data."
  (interactive)
  (geo-ip-lat-lon-loc-ip #'geo-ip--set-calendar-vars))

(provide 'geo-ip)

;;; geo-ip.el ends here
