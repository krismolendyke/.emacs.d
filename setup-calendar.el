(require 'geo-ip)
(require 'url)

(add-hook
 'calendar-load-hook
 #'(lambda ()
     ;; Default location Philly.
     (setq
      calendar-latitude 39.9            ; 39.9525
      calendar-longitude -75.1          ; -75.163
      calendar-location-name "Philadelphia, PA")

     ;; Attempt to set location with a geo-ip query.
     (lat-lon-loc-ip
      #'(lambda (lat lon loc ip)
          (setq
           calendar-latitude lat
           calendar-longitude lon
           calendar-location-name loc)))))

(provide 'setup-calendar)
