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


;; LDATESPEC := (MONTH DAY YEAR LEAP)
;;            | (MONTH DAY YEAR)

(require 'lunar-ko-mdays)

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


(defun lunar-ko-month-days (year month &optional leap)
  "Return the number of days in Korean lunar date YEAR MONTH

If the optional LEAP is non-nil, MONTH is considered as a leap month.

If LEAP is non-nil and the actual YEAR has no leap month
in MONTH, LEAP is ignored."
  (let* ((val (gethash year korean-lunar-months))
         (lmon (car val))
         (days (cdr val)))
    (if lmon
        (cond ((< month lmon) (aref days (1- month)))
              ((> month lmon) (aref days month))
              ((= month lmon) (aref days (- month (if leap 0 1)))))
      (aref days (1- month)))))
    
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


(defun lunar-ko-kld (ldatespec)
  (+ (* (lunar-ko-year ldatespec) 100000)
     (* (lunar-ko-month ldatespec) 1000)
     (if (lunar-ko-leap? ldatespec) 100 0)
     (lunar-ko-day ldatespec)))

(defun lunar-ko-date (kld)
  (list (/ (mod kld 100000) 1000)
        (mod kld 100)
        (/ kld 100000)
        (if (= (/ (mod kld 1000) 100) 1)
            t)))


(defun lunar-ko-days-to (ldatespec)
  "Return number of days from the beginning of the lunar year to LDATESPEC.

The date of LDATESPEC is not counted."
  (let* ((val (gethash (lunar-ko-year ldatespec) korean-lunar-months))
         (vec (cdr val))
         (idx (lunar-ko-month-index ldatespec))
         (days 0))
    (1- (+ (dotimes (i idx days)
             (setq days (+ days (aref vec i))))
           (lunar-ko-day ldatespec)))))


(defun lunar-ko-days-from (ldatespec)
  "Return number of days from LDATESPEC to the end of the lunar year.

The date of LDATESPEC is counted."
  (let* ((val (gethash (lunar-ko-year ldatespec) korean-lunar-months))
         (vec (cdr val))
         (idx (lunar-ko-month-index ldatespec))
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
