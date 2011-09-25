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

(defconst korean-celestial-stem [("갑" . "甲")
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

(defconst korean-terrestrial-branch [("자" . "子")
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

(defun lunar-ko-sexagesimal-index (index)
  "Get the Korean sexagesimal name from INDEX.

INDEX should be 0 or less than 60.   It returns (KOREAN . CHINESE),
where KOREAN is the korean name and CHINESE is the chinese characters."
  (let ((cel (aref korean-celestial-stem (mod index 10)))
        (ter (aref korean-terrestrial-branch (mod index 12))))
    (cons (concat (car cel) (car ter))
          (concat (cdr cel) (cdr ter)))))

(defun lunar-ko-sexagesimal-year (ldatespec)
  "Return the name of the Korean sexagesimal name.

The returned value has the form '(KOREAN-NAME . CHINESE-NAME)'"
  (lunar-ko-sexagesimal-index
   (mod (- (+ (mod (lunar-ko-year ldatespec) 60) 60) 4) 60)))

(defun lunar-ko-sexagesimal-month (ldatespec)
  (let ((year (lunar-ko-year ldatespec))
        (month (lunar-ko-month ldatespec)))
    (lunar-ko-sexagesimal-index
     (- (+ (* (mod (- (+ (mod year 5) 5) 3) 5) 12) month) 11))))

(defun lunar-ko-sexagesimal-day (ldatespec &optional cache)
  (let* ((entry (or cache
                    (gethash (lunar-ko-year ldatespec) 
                             korean-lunar-months)))
         (base (cadr entry)))
    (lunar-ko-sexagesimal-index
     (mod (+ (mod (lunar-ko-days-to ldatespec entry) 60) base) 60))))
              

(defun lunar-ko-sexagesimal-name (ldatespec &optional chinese cache)
  (let ((year (lunar-ko-sexagesimal-year ldatespec))
        (month (lunar-ko-sexagesimal-month ldatespec))
        (day (lunar-ko-sexagesimal-day ldatespec cache)))
    (if chinese
        (format "%s(%s)년 %s(%s)월 %s(%s)일"
                (car year) (cdr year)
                (car month) (cdr month)
                (car day) (cdr day))
      (format "%s년 %s월 %s일"
              (car year) (car month) (car day)))))



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
         (vec (cddr entry))
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
                   (gethash (lunar-ko-year ldatespec) 
                            korean-lunar-months))))
    (aref (cddr entry) (lunar-ko-month-index ldatespec entry))))



(defun lunar-ko-impl-days (impl &optional cache)
  "Return the number of days in the lunar month from internal
representation, IMPL.

The optional CACHE specifies the pre-acquired hash entry if any."
  (let ((entry (or cache
                   (gethash (lunar-ko-year impl) korean-lunar-months))))
    (aref (cddr entry) (lunar-ko-month impl))))

    
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
             (vec (cddr entry))
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


(defun lunar-ko-days-to (ldatespec &optional cache)
  "Return number of days from the beginning of the lunar year to LDATESPEC.

The date of LDATESPEC is not counted."
  (let* ((entry (or cache
                    (gethash (lunar-ko-year ldatespec) 
                             korean-lunar-months)))
         (vec (cddr entry))
         (idx (lunar-ko-month-index ldatespec entry))
         (days 0))
    (1- (+ (dotimes (i idx days)
             (setq days (+ days (aref vec i))))
           (lunar-ko-day ldatespec)))))


(defun lunar-ko-days-from (ldatespec)
  "Return number of days from LDATESPEC to the end of the lunar year.

The date of LDATESPEC is counted."
  (let* ((entry (gethash (lunar-ko-year ldatespec) korean-lunar-months))
         (vec (cddr entry))
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
                    (reduce #'+ (cddr (gethash (+ i start)
                                               korean-lunar-months))))))))


;; These two variables governs `calenday-day-name' to return the
;; day name of the week.
;;
;; It turns out that modifying `calendar-day-name-array' and
;; `calendar-day-abbrev-array' is not good idea.  If we do, the layout
;; of calendar will be not aligned correctly, unless the Korean glyphs
;; always have double width of the western glyph.
(when nil
  (setq calendar-day-abbrev-array [ nil nil nil nil nil nil nil ]
        calendar-day-name-array ["일요일" "월요일" "화요일" "수요일" 
                                 "목요일" "금요일" "토요일"]))

(defconst cal-korea-short-day-names
  ["일" "월" "화" "수" "목" "금" "토"]
  "The abbreviated Korean week name")
;;(defconst cal-korea-month-names
;;  ["일월" "이월" "삼월" "사월" "오월" "유월" "칠월" "팔월" "구월" "시월" "십일월" "십이월"]
;;  "The Korean month name")

(setq calendar-month-name-array
      ["1월" "2월" "3월" "4월" "5월" "6월" "7월" "8월" "9월" "10월" "11월" "12월"])
      
(defun cal-korea-day-name (date)
  "Korean day name in a week, like \"월요일\"."
  (concat (aref cal-korea-short-day-names (calendar-day-of-week date))
          "요일"))

(defun cal-korea-month-name (month &optional abbrev)
  (if abbrev
      (format "%d월" month)
    (aref cal-korea-month-names (1- month))))

(defun cal-korea-x-calendar-display-form (date)
  (if (equal date '(0 0 0))
      ""
    (format "%04d년 %02d월 %02d일 %s"
            (calendar-extract-year date)
            (calendar-extract-month date)
            (calendar-extract-day date)
            (cal-korea-day-name date))))

(setq calendar-date-display-form
      '((cal-korea-x-calendar-display-form 
         (mapcar (lambda (el) (string-to-number el))
                   (list month day year)))))

(setq calendar-mode-line-format
      (list
       (calendar-mode-line-entry 'calendar-scroll-right "이전 달" "<")
       "달력"
       (concat
        (calendar-mode-line-entry 'calendar-goto-info-node "달력 도움말"
                                  nil "도움말")
        " / "
        (calendar-mode-line-entry 'calendar-other-month "다른 달로 가기"
                                  nil "다른 달")
        " / "
        (calendar-mode-line-entry 'calendar-goto-today "오늘로 가기"
                                  nil "오늘"))
       ;;'(calendar-date-string (calendar-current-date) t)
       '(calendar-date-string date t)
       (calendar-mode-line-entry 'calendar-scroll-left "다음 달" ">")))

(add-hook 'calendar-move-hook 'calendar-update-mode-line)
(add-hook 'calendar-initial-window-hook 'calendar-update-mode-line)


;;
;; WARNING: Until Emacs provides a way to customize the calendar header,
;; there's no way to display custom calendar header.
;;
;; This `calendar-generate-month' is directly copied from lisp/calender.el
;; of "GNU Emacs 23.3.1 (i686-pc-linux-gnu, GTK+ Version 2.22.1)", 
;; and modified to use custom calendar header.
;;
;; See also http://debbugs.gnu.org/cgi/bugreport.cgi?bug=9510
;;

(defcustom calendar-month-header-format
  (list '(format "%s %d" (calendar-month-name month) year))
  "The header line of the calendar.
This is a list of items that evaluate to strings.  During
evaluation, the variable `month' and `year' are available as the
month and year of the calendar.")

(defun calendar-generate-month (month year indent)
  "Produce a calendar for MONTH, YEAR on the Gregorian calendar.
The calendar is inserted at the top of the buffer in which point is currently
located, but indented INDENT spaces.  The indentation is done from the first
character on the line and does not disturb the first INDENT characters on the
line."
  (let ((blank-days                     ; at start of month
         (mod
          (- (calendar-day-of-week (list month 1 year))
             calendar-week-start-day)
          7))
         (last (calendar-last-day-of-month month year))
         (trunc (min calendar-intermonth-spacing
                     (1- calendar-left-margin)))
         (day 1)
         string)
   (goto-char (point-min))
   (calendar-move-to-column indent)
   (insert
    (calendar-string-spread
     (mapcar 'eval calendar-month-header-format)
     ?\s calendar-month-digit-width))
   (calendar-ensure-newline)
   (calendar-insert-at-column indent calendar-intermonth-header trunc)
   ;; Use the first two characters of each day to head the columns.
   (dotimes (i 7)
     (insert
      (progn
        (setq string
              (calendar-day-name (mod (+ calendar-week-start-day i) 7) nil t))
        (if enable-multibyte-characters
            (truncate-string-to-width string calendar-day-header-width)
          (substring string 0 calendar-day-header-width)))
      (make-string (- calendar-column-width calendar-day-header-width) ?\s)))
   (calendar-ensure-newline)
   (calendar-insert-at-column indent calendar-intermonth-text trunc)
   ;; Add blank days before the first of the month.
   (insert (make-string (* blank-days calendar-column-width) ?\s))
   ;; Put in the days of the month.
   (dotimes (i last)
     (setq day (1+ i))
     ;; TODO should numbers be left-justified, centered...?
     (insert (format (format "%%%dd%%s" calendar-day-digit-width) day
                     (make-string
                      (- calendar-column-width calendar-day-digit-width) ?\s)))
     ;; 'date property prevents intermonth text confusing re-searches.
     ;; (Tried intangible, it did not really work.)
     (set-text-properties
      (- (point) (1+ calendar-day-digit-width)) (1- (point))
      `(mouse-face highlight help-echo ,(eval calendar-date-echo-text)
                   date t))
     (when (and (zerop (mod (+ day blank-days) 7))
                (/= day last))
       (calendar-ensure-newline)
       (setq day (1+ day))              ; first day of next week
       (calendar-insert-at-column indent calendar-intermonth-text trunc)))))

;;(setq calendar-month-header-format
;;      (list '(format "%d년 %s" year (cal-korea-month-name month))))

(setq calendar-month-header-format
      (list '(format "%d년 %s" year (calendar-month-name month))))


(provide 'lunar-ko)
;;; lunar-ko.el ends here
