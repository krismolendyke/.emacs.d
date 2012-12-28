;;; geo-ip.el --- Query DuckDuckGo for external IP and lat/lon

;;; Commentary:

;; Several functions for determining location based on external IP
;; address.  lat-lon-loc-ip should be the only externally useable
;; function as the rest are simple regular expression based text
;; extraction helper functions.

;;; Code:

;; Extract and return the location name string.
(defun location-name ()
  (save-excursion
    (let ((beginning (search-forward-regexp "mapquest\\.com/\\?q=[0-9.,-]+\">"))
          (end (1- (search-forward-regexp "<"))))
      (buffer-substring-no-properties beginning end))))

;; Extract and return the numeric latitude/longitude pair.
(defun latitude-longitude ()
  (save-excursion
    (let ((beginning (search-forward-regexp "mapquest\\.com/\\?q="))
          (end (1- (search-forward-regexp "\""))))
      (mapcar
       #'(lambda (x) (string-to-number x))
       (split-string (buffer-substring-no-properties beginning end) ",")))))

;; Extract and return the IP address string.
(defun ip-address ()
  (save-excursion
    (let ((beginning
           (search-forward-regexp
            "Your[[:space:]]+IP[[:space:]]+address[[:space:]]+is[[:space:]]+"))
          (end (1- (search-forward-regexp " "))))
      (buffer-substring-no-properties beginning end))))

(defun lat-lon-loc-ip (callback)
  "Make a request to DuckDuckGo for the external IP address of
this machine.  Extract latitude, longitude, location name and IP
address from the response and pass them as arguments to CALLBACK."
  (url-retrieve
   "http://duckduckgo.com/?q=ip"
   #'(lambda (status callback)
       (unwind-protect
           (let* ((lat-lon (latitude-longitude))
                  (lat (car lat-lon))
                  (lon (cadr lat-lon))
                  (loc (location-name))
                  (ip (ip-address)))
             (funcall callback lat lon loc ip))
         (kill-buffer)))
   `(,callback)))

(provide 'geo-ip)
