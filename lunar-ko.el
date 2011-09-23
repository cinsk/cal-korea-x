;;; lunar-ko.el --- Utilities for Korean lunar calendar

;; Copyright (C) 2011  Seong-Kook Shin

;; Author: Seong-Kook Shin <cinsky@gmail.com>
;; Keywords: calendar, lisp, local, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;;
;; First of all, I don't have enough knowledge of lunar calendar
;; system of either Chinese lunar calendar or Korean lunar calendar.
;; If you find any mistake in my code/documentation, please let me
;; know.
;; 

;;
;; AFAIK, the Korean lunar calendar is almost the same as Chinese
;; lunar calendar.  They are different because, the Chinese one is
;; based on the astronomical calculation is carried out for the
;; meridian 120 degrees east of Greenwich, where the Korean one is
;; based on for the meridian 135 degrees east of Greenwich.
;;
;; For example, the Chinese lunar new year's day in 1997 was Feb. 7th
;; 1997 where the Korean lunar new year's day in 1997 was Feb. 8th
;; 1997.
;;
;; AFAIK, there is no absolute perfect formula to convert the
;; solar(gregorian) date into (Korean?) lunar date.  The official
;; Korean lunar calendar is issued by Korea Astronomy and Space
;; Science Institute, and it seems that they provides only web-based
;; converting tools for the lunar dates from (1 1 1391) to (11 18
;; 2050) .
;;

;; LDATESPEC := (MONTH DAY YEAR LEAP)
;;            | (MONTH DAY YEAR)
;;
;; MONTH := 1-12
;; DAY   := 1-30
;; YEAR  := 1391-2050
;; LEAP  := t or nil

(require 'lunar-ko-mdays)
(require 'lunar-ko-cache)

(defconst lunar-ko-celestial-stem
  [("갑" . "甲")
   ("을" . "乙")
   ("병" . "丙")
   ("정" . "丁")
   ("무" . "戊")
   ("기" . "己")
   ("경" . "庚")
   ("신" . "辛")
   ("임" . "壬")
   ("계" . "癸")]
   "Korean celestial stem (identical to that of Chinese)")

(defconst lunar-ko-terrestrial-branch [("자" . "子")
                                       ("축" . "丑")
                                       ("인" . "寅")
                                       ("묘" . "卯")
                                       ("진" . "辰")
                                       ("사" . "巳")
                                       ("오" . "午")
                                       ("미" . "未")
                                       ("신" . "申")
                                       ("유" . "酉")
                                       ("술" . "戌")
                                       ("해" . "亥")]
   "Korean terrestrial branch (identical to that of Chinese)")


(defun gregorian-to-julian (date)
  "Convert gregorian date in the form of (MONTH DAY YEAR) to
Julian day number.  See also `calendar-astro-date-string'."
  (calendar-astro-from-absolute
   (calendar-absolute-from-gregorian date)))
  
(defun julian-to-gregorian (date)
  "Convert Julian day number to the gregorian date in the form
of (MONTH DAY YEAR).  See also `calendar-astro-goto-day-number'."
  (calendar-gregorian-from-absolute 
   (floor (calendar-astro-to-absolute date))))

(defun lunar-ko-find-nearest (value type &optional from to)
  "Find the nearest and less-than-or-equal entry of VALUE from
`korean-lunar-cache'.

VALUE is an astronomical julian day number if TYPE is :solar, or
is a Korean lunar day number if TYPE is :lunar."
  (let* ((begin (if from from 0))
         (end (if to to (length korean-lunar-cache)))
         mid)
    (flet ((cmp (type lhs rhs-pair)
                (let ((rhs (if (eq type :solar)
                               (car rhs-pair) (cdr rhs-pair))))
                  (- lhs rhs))))
      (let ((min_pair (aref korean-lunar-cache 0))
            (max_pair (aref korean-lunar-cache 
                            (1- (length korean-lunar-cache)))))
        (if (or (< (cmp type value min_pair) 0)
                (> (cmp type value max_pair) 0))
            (error "VALUE out of range [%S - %S]" min_pair max_pair)))
      (catch 'found
        (while (<= begin end)
          (setq mid (/ (+ begin end) 2))
          (let ((pair (aref korean-lunar-cache mid)))
            ;;(print (format "%S-%S-%S" begin mid end))
            (let ((ret (cmp type value pair)))
              (cond ((< ret 0) (setq end (1- mid)))
                    ((> ret 0) (setq begin (1+ mid)))
                    ((= ret 0) (throw 'found pair)))))))
      ;;(print (format "mid: %S" mid))
      (let ((pair (aref korean-lunar-cache mid)))
        (if (< (cmp type value pair) 0)
            (aref korean-lunar-cache (1- mid))
          pair)))))

(defun lunar-ko-sexagenary-name (index)
  (let ((cel (aref lunar-ko-celestial-stem (mod index 10)))
        (ter (aref lunar-ko-terrestrial-branch (mod index 12))))
    (cons (concat (car cel) (car ter))
          (concat (cdr cel) (cdr ter)))))

(defun lunar-ko-year (ldatespec)
  (caddr ldatespec))
(defun lunar-ko-month (ldatespec)
  (car ldatespec))
(defun lunar-ko-day (ldatespec)
  (cadr ldatespec))


;;; TODO: recheck any code that uses `lunar-ko-leap' is correct!
(defun lunar-ko-leap (ldatespec)
  (if (and (= (length ldatespec) 4)
           (nth 3 ldatespec))
      (lunar-ko-month ldatespec)
    nil))

(defun lunar-ko-leap? (ldatespec)
  (and (= (length ldatespec) 4)
       (nth 3 ldatespec)))


(defun lunar-ko-month-index (ldatespec &optional cache)
  "Return the month index of the vector in `korean-lunar-months' as
in LDATESPEC.

The optional CACHE specifies the pre-acquired hash entry if any."
  (let* ((entry (or cache 
                    (gethash (lunar-ko-year ldatespec) korean-lunar-months)))
         (lmon (car entry))
         (vec (cdr entry))
         (month (lunar-ko-month ldatespec)))
    (assert (< month 13) t "out of range: %S")
    (assert (> month 0) t "out of range: %S")
    (cond ((null lmon) (1- month))
          ((< month lmon) (1- month))
          ((and (= month lmon) (not (lunar-ko-leap? ldatespec))) (1- month))
          (t month))))

(defun lunar-ko-month-days (ldatespec &optional cache)
  "Return the number of days in Korean lunar date, LDATESPEC

The optional CACHE specifies the pre-acquired hash entry if any."
  (let ((entry (or cache
                   (gethash (lunar-ko-year ldatespec) korean-lunar-months))))
    (aref (cdr entry) (lunar-ko-month-index ldatespec entry))))

(defun lunar-ko-impl-days (impl &optional cache)
  "Return the number of days in the lunar month from internal
representation, IMPL.

The optional CACHE specifies the pre-acquired hash entry if any."
  (let ((entry (or cache
                   (gethash (lunar-ko-year impl) korean-lunar-months))))
    (aref (cdr entry) (lunar-ko-month impl))))

    
(defun lunar-ko-date-to-impl (ldatespec &optional cache)
  "Convert LDATESPEC into internal lunar format.

The internal form is like (MONTH-INDEX DAY YEAR), where MONTH-INDEX
is the index value of the vector in `korean-lunar-months' (0-12)."
  (list (lunar-ko-month-index ldatespec)
        (lunar-ko-day ldatespec)
        (lunar-ko-year ldatespec)))

(defun lunar-ko-impl-to-date (impl &optional cache)
  "Convert the internal lunar value into LDATESPEC.

The internal form is like (MONTH-INDEX DAY YEAR), where MONTH-INDEX
is the index value of the vector in `korean-lunar-months' (0-12)."
  (let* ((month (lunar-ko-month impl))
         (day (lunar-ko-day impl))
         (year (lunar-ko-year impl))
         (entry (or cache
                    (gethash year korean-lunar-months)))
         (leap-month (car entry)))
    (cond ((null leap-month) (list (1+ month) day year))
          ((< month leap-month) (list (1+ month) day year))
          ((= month leap-month) (list (1+ month) day year t))
          ((> month leap-month) (list month day year)))))

(defun lunar-ko-advance (ldatespec days)
  "Return the lunar date, advanced DAYS days from LDATESPEC."
  (let ((date (lunar-ko-date-to-impl ldatespec)))
    (while (> days 0)
      (let* ((year (lunar-ko-year date))
             (entry (gethash year korean-lunar-months))
             (vec (cdr entry))
             (midx (lunar-ko-month date))
             (remain-days (- (aref vec midx) (lunar-ko-day date))))
        (let ((mdays (lunar-ko-impl-days date))
              (day (+ (lunar-ko-day date) days)))
          (if (<= day mdays)
              ;; If there's no carry to MONTH
              (setq days 0
                    date (list midx day year)) ; FIN
            ;; If there's carry from DAY to MONTH
            (setq days (- days (- mdays (lunar-ko-day date)))
                  day 0
                  midx (1+ midx))
            (if (< midx (length vec))
                ;; If there's no carry to YEAR
                (setq date (list midx day year)) ; no need to update the year
              ;; If there's carry from MONTH to YEAR
              (setq date (list 0 day (1+ year))))))))
    (lunar-ko-impl-to-date date)))


(defun lunar-ko-valid? (ldatespec)
  "Return t if the lunar date LDATESPEC is actually existed."
  (let* ((val (gethash (lunar-ko-year ldatespec) korean-lunar-months))
         (lmon (car val)))
    (and (if (lunar-ko-leap? ldatespec)
             (equal (lunar-ko-leap ldatespec) lmon)
           t)
         (<= (lunar-ko-day ldatespec)
             (lunar-ko-month-days (lunar-ko-year ldatespec)
                                  (lunar-ko-month ldatespec)
                                  (lunar-ko-leap ldatespec))))))


(defun lunar-ko-month-index (ldatespec)
  (let* ((val (gethash (lunar-ko-year ldatespec) korean-lunar-months))
         (mon (lunar-ko-month ldatespec))
         (lmon (car val))
         (mdays (cdr val)))
    (if lmon
        (cond ((< mon lmon) (1- mon))
              ((> mon lmon) mon)
              ((lunar-ko-leap ldatespec) mon)
              (t (1- mon)))
      (1- mon))))


(defun lunar-ko-date-to-kld (ldatespec)
  (+ (* (lunar-ko-year ldatespec) 100000)
     (* (lunar-ko-month ldatespec) 1000)
     (if (lunar-ko-leap? ldatespec) 100 0)
     (lunar-ko-day ldatespec)))

(defun lunar-ko-kld-to-date (kld)
  (list (/ (mod kld 100000) 1000)
        (mod kld 100)
        (/ kld 100000)
        (if (= (/ (mod kld 1000) 100) 1)
            t)))


(defun lunar-ko-lunar-date (solar-date)
  "Convert the solar date into the Korean lunar date."
  (let* ((ajd (gregorian-to-julian solar-date))
         (pair (lunar-ko-find-nearest ajd :solar)))
    (lunar-ko-advance (lunar-ko-kld-to-date (cdr pair))
                      (round (- ajd (car pair))))))


(defun lunar-ko-days-to (ldatespec)
  "Return number of days from the beginning of the lunar year to LDATESPEC.

The date of LDATESPEC is not counted."
  (let* ((entry (gethash (lunar-ko-year ldatespec) korean-lunar-months))
         (vec (cdr entry))
         (idx (lunar-ko-month-index ldatespec entry))
         (days 0))
    (1- (+ (dotimes (i idx days)
             (setq days (+ days (aref vec i))))
           (lunar-ko-day ldatespec)))))


(defun lunar-ko-days-from (ldatespec)
  "Return number of days from LDATESPEC to the end of the lunar year.

The date of LDATESPEC is counted."
  (let* ((entry (gethash (lunar-ko-year ldatespec) korean-lunar-months))
         (vec (cdr entry))
         (idx (lunar-ko-month-index ldatespec entry))
         (days 0))
    (+ (- (aref vec idx) (lunar-ko-day ldatespec)) 
       1
       (dotimes (i (- (length vec) idx 1) days)
         (setq days (+ days (aref vec (+ i idx 1))))))))


(defun lunar-ko-days-range-year (year1 year2)
  "Count days from YEAR1 and YEAR2.

YEAR1 is inclusive, and YEAR2 is exclusive."
  (let ((start (min year1 year2))
        (end (max year1 year2))
        (days 0))
    (dotimes (i (- end start) days)
      (setq days (+ days
                    (reduce #'+ (cdr (gethash (+ i start)
                                              korean-lunar-months))))))))

(provide 'lunar-ko)
;;; lunar-ko.el ends here
