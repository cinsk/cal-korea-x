;;; cal-korea-x.el --- Utilities for Korean lunar calendar -*- coding: utf-8 -*-

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

;; in ~/diary file
;; -----------------------------------------------------------------------------
;; %%(diary-lunar-date 9 16 2013) TEST 음력 날짜

;; %%(diary-lunar-anniversary 2 27) TEST 생일(%s)

;; %%(diary-lunar-anniversary 9 27 t) TEST 윤달 생일(%s)

;; %%(diary-solar-24term "소한") 소한(小寒)
;; %%(diary-solar-24term "대한") 대한(大寒)
;; %%(diary-solar-24term "입춘") 입춘(立春)
;; %%(diary-solar-24term "우수") 우수(雨水)
;; %%(diary-solar-24term "경칩") 경칩(驚蟄)
;; %%(diary-solar-24term "춘분") 춘분(春分)
;; %%(diary-solar-24term "청명") 청명(淸明)
;; %%(diary-solar-24term "곡우") 곡우(谷雨)
;; %%(diary-solar-24term "입하") 입하(立夏)
;; %%(diary-solar-24term "소만") 소만(小滿)
;; %%(diary-solar-24term "망종") 망종(芒種)
;; %%(diary-solar-24term "하지") 하지(夏至)
;; %%(diary-solar-24term "소서") 소서(小暑)
;; %%(diary-solar-24term "대서") 대서(大暑)
;; %%(diary-solar-24term "입추") 입추(立秋)
;; %%(diary-solar-24term "처서") 처서(處暑)
;; %%(diary-solar-24term "백로") 백로(白露)
;; %%(diary-solar-24term "추분") 추분(秋分)
;; %%(diary-solar-24term "한로") 한로(寒露)
;; %%(diary-solar-24term "상강") 상강(霜降)
;; %%(diary-solar-24term "입동") 입동(立冬)
;; %%(diary-solar-24term "소설") 소설(小雪)
;; %%(diary-solar-24term "대설") 대설(大雪)
;; %%(diary-solar-24term "동지") 동지(冬至)
;; -----------------------------------------------------------------------------

(require 'cl)
(require 'calendar)
(require 'holidays)
(require 'solar)
(require 'cal-julian)
(require 'lunar-ko-mdays)
(require 'lunar-ko-cache)

(defvar cal-korea-x-use-korean-month-name t)

(defvar cal-korea-x-use-korean-week-name nil)

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

(defconst cal-korea-x-solar-term-name
  ["소한" "대한" "입춘" "우수" "경칩" "춘분" "청명" "곡우" "입하" "소만" "망종" "하지"
   "소서" "대서" "입추" "처서" "백로" "추분" "한로" "상강" "입동" "소설" "대설" "동지"]
  "24 solar terms(in korean).")

(defconst cal-korea-x-korean-holidays
  '((holiday-fixed 1 1          "신정")
    (holiday-lunar-ko 1 nil 1   "설날" -1)
    (holiday-lunar-ko 1 nil 1   "설날")
    (holiday-lunar-ko 1 nil 1   "설날" 1)
    (holiday-fixed 3 1          "3.1절")
    (holiday-lunar-ko 4 nil 8   "석가탄신일")
    (holiday-fixed 5 5          "어린이날")
    (holiday-fixed 6 6          "현충일")
    (holiday-fixed 8 15         "광복절")
    (holiday-fixed 10 3         "개천절")
    (holiday-fixed 10 9         "한글날")
    (holiday-lunar-ko 8 nil 15  "추석" -1)
    (holiday-lunar-ko 8 nil 15  "추석")
    (holiday-lunar-ko 8 nil 15  "추석" 1)
    (holiday-fixed 12 25        "성탄절"))
  "Pre-define Korean public holidays.")



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
  ;; WARNING!  There is difference between Emacs julian date function
  ;; and others(sqlite3, ruby, etc.) around before 1582-10-*.  Since
  ;; `korean-lunar-cache' is generated by Ruby, we cannot convert the
  ;; julian date in `korean-lunar-cache' if DATE is prior to
  ;; 1582-10-*.
  ;;(assert (> date 2299177) "out of range: %S")
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



;;
;; LDATESPEC is the internal Korean lunar date representation in number form.
;; For example, the Korean lunar date, "April(leap) 8th 1947" is represented
;; as 194704108.
;;
;; 194704108
;; ^^^^          lunar year
;;     ^^        lunar month
;;       ^       1 if the lunar month is leap month, otherwise 0.
;;        ^^     lunar day
;;

(defun lunar-ko-new-ldatespec (month day year &optional leap)
  (assert (> year 1390) t "out of range: %S")
  (assert (<= year 2050) t "out of range: %S")
  (assert (> month 0) t "out of range: %S")
  (assert (<= month 12) t "out of range: %S")
  (assert (> day 0) t "out of range: %S")
  (assert (<= day 30) t "out of range: %S")
  (+ (* year 100000)
     (* month 1000)
     (* (if leap 1 0) 100)
     day))

(defun lunar-ko-year (ldatespec)
  (/ ldatespec 100000))

(defun lunar-ko-month (ldatespec)
  (/ (mod ldatespec 100000) 1000))

(defun lunar-ko-day (ldatespec)
  (mod ldatespec 100))

(defun lunar-ko-leapmonth (ldatespec)
  "Return the leap month(1-12) if any"
  (if (lunar-ko-leap? ldatespec)
      (lunar-ko-month ldatespec)))

(defun lunar-ko-leap? (ldatespec)
  "Return t if the year has leap month"
  (if (= (/ (mod ldatespec 1000) 100) 1)
      t))

;;; TODO: recheck any code that uses `lunar-ko-leap' is correct!



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
                   (gethash (nth 2 impl) korean-lunar-months))))
    (aref (cddr entry) (nth 0 impl))))


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
  (let* ((midx (nth 0 impl))
         (day (nth 1 impl))
         (year (nth 2 impl))
         (entry (or cache
                    (gethash year korean-lunar-months)))
         (leap-month (car entry)))
    (cond ((null leap-month) (lunar-ko-new-ldatespec (1+ midx) day year))
          ((< midx leap-month) (lunar-ko-new-ldatespec (1+ midx) day year))
          ((= midx leap-month) (lunar-ko-new-ldatespec midx day year t))
          ((> midx leap-month) (lunar-ko-new-ldatespec midx day year)))))

(defun lunar-ko-advance (ldatespec days)
  "Return the lunar date, advanced DAYS days from LDATESPEC."
  (let* ((date (lunar-ko-date-to-impl ldatespec)))
    (setq days (round days))
    (while (> days 0)
      (let* ((year (nth 2 date))
             (entry (gethash year korean-lunar-months))
             (vec (cddr entry))
             (midx (nth 0 date))
             (remain-days (- (aref vec midx) (nth 1 date))))
        (let ((mdays (lunar-ko-impl-days date))
              (day (+ (nth 1 date) days)))
          (if (<= day mdays)
              ;; If there's no carry to MONTH
              (setq days 0
                    date (list midx day year)) ; FIN
            ;; If there's carry from DAY to MONTH
            (setq days (- days (- mdays (nth 1 date)))
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
             (equal (lunar-ko-leap? ldatespec) lmon)
           t)
         (<= (lunar-ko-day ldatespec)
             (lunar-ko-month-days ldatespec)))))


(defun lunar-ko-lunar-date (solar-date)
  "Convert the solar date into the Korean lunar date."
  (let* ((ajd (gregorian-to-julian solar-date))
         (pair (lunar-ko-find-nearest ajd :solar)))
    (lunar-ko-advance (cdr pair)
                      (round (- ajd (car pair))))))


(defun lunar-ko-solar-date (ldatespec)
  "Convert the Korean lunar date LDATESPEC into Gregorian date.

Due to the limitation of `julian-to-gregorian', LDATESPEC should
be no less than 1582-11-01."
  (let ((base (lunar-ko-find-nearest ldatespec :lunar)))
    (julian-to-gregorian (+ (car base)
                            (- (lunar-ko-days-to ldatespec)
                               (lunar-ko-days-to (cdr base)))))))


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

(defun cal-korean-lunar-print-date (&optional date)
  "Show the Chinese date equivalents of date."
  (interactive "P")
  (and (called-interactively-p 'interactive)
       (message "Computing Korean lunar date..."))
  (let* ((ldatespec (lunar-ko-lunar-date (calendar-cursor-to-date t)))
         (year (lunar-ko-year ldatespec))
         (month (lunar-ko-month ldatespec))
         (day (lunar-ko-day ldatespec))
         (leap (if (lunar-ko-leap? ldatespec) " (윤)" ""))
         (datestr (format "%d년 %d월 %d일%s" year month day leap)))
    (if (called-interactively-p 'interactive)
        (let ((kanji (lunar-ko-sexagesimal-name ldatespec t)))
          (message "음력: %s, %s" datestr kanji))
      datestr)))

(defun cal-korean-goto-date (ldate &optional noecho)
  "Move cursor to Korean lunar date DATE.
Echo Korean lunar date unless NOECHO is non-nil."
  (interactive
   (let* ((now (lunar-ko-lunar-date (calendar-current-date)))
          (year (calendar-read
                 "Year in Korean lunar date: "
                 (lambda (x) (and (> x 1390) (<= x 2050)))
                 (number-to-string (lunar-ko-year now))))
          (month (calendar-read
                  "Month in Korean lunar date: "
                  (lambda (x) (and (> x 0) (<= x 12)))
                  (number-to-string (lunar-ko-month now))))
          (leap? (calendar-read
                 "Is it leap month? [y/N] "
                 (lambda (x) (or (eq x 'y) (eq x 'Y)
                                 (eq x 'n) (eq x 'N)))
                 "N"))
          (day (calendar-read
                "Day in korean lunar date: "
                (lambda (x) (and (> x 0) (<= x 30)))
                (number-to-string (lunar-ko-day now))))
          (leap (if (or (eq leap? 'y) (eq leap? 'Y))
                    t
                  nil)))
     (list (list month day year leap))))
  (calendar-goto-date (lunar-ko-solar-date (apply 'lunar-ko-new-ldatespec
                                                  ldate)))
  (or noecho (cal-korean-lunar-print-date)))

(eval-after-load "calendar"
  '(progn
     (define-key calendar-mode-map "pK" 'cal-korean-lunar-print-date)
     (define-key calendar-mode-map "gK" 'cal-korean-goto-date)))


(when cal-korea-x-use-korean-month-name
  (setq calendar-month-name-array ["1월" "2월" "3월" "4월" "5월" "6월" "7월" "8월" "9월" "10월" "11월" "12월"]))

(when cal-korea-x-use-korean-week-name
  (setq calendar-day-abbrev-array [ "일" "월" "화" "수" "목" "금" "토" ]
        calendar-day-name-array ["일요일" "월요일" "화요일" "수요일" "목요일" "금요일" "토요일"]))

(defconst cal-korea-short-day-names
  ["일" "월" "화" "수" "목" "금" "토"]
  "The abbreviated Korean week name")

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


(defun holiday-lunar-ko (lunar-month leap-month lunar-day string
                                     &optional dist)
  "Like `holiday-fixed', but with Korean lunar date in LUNAR-MONTH, LUNAR-DAY.

LEAP-MONTH is t if LUNAR-MONTH is the leap month.  DIST denotes that
the day distance(offset) from the lunar date.  For example:

DIST = 0 (or nil), the lunar date given,
DIST = 1, the next day of the lunar date,
DIST = -1, the previous day of the lunar date.

DIST is useful to make a holiday period, like Korean New Year's
day (three holidays).  The actual holidays are from the previous
day of New Year's day to the next day of New Year's day.  Thus,

  (holiday-lunar-ko 1 nil 1 \"New Year's Day\" -1)
  (holiday-lunar-ko 1 nil 1 \"New Year's Day\")
  (holiday-lunar-ko 1 nil 1 \"New Year's Day\" +1)

will do."
  (let* ((pivot (lunar-ko-solar-date
                 (lunar-ko-new-ldatespec lunar-month lunar-day
                                         displayed-year leap-month)))
         (date (julian-to-gregorian (+ (gregorian-to-julian pivot)
                                       (or dist 0)))))
    (holiday-fixed (nth 0 date) (nth 1 date)
                   (format "%s%s" string
                           (if (= (or dist 0) 0)
                               ""
                             (format " (%+d)" dist))))))

;; =============================================================================
;; for korean 24 terms

(defun cal-korea-x-gregorian-from-astro (a)
  (calendar-gregorian-from-absolute
   (floor (calendar-astro-to-absolute a))))

(defun cal-korea-x-astro-from-gregorian (g)
  (calendar-astro-from-absolute
   (calendar-absolute-from-gregorian g)))

(defun cal-korea-x-next-solar-term (date)
  "Return next solar term's data after DATE.
Each solar term is separated by 15 longtitude degrees or so, plus an
extra day appended."
  (cal-korea-x-gregorian-from-astro
    (solar-date-next-longitude
     (cal-korea-x-astro-from-gregorian
      (calendar-gregorian-from-absolute
       (1+ (calendar-absolute-from-gregorian date))))
     15)))

(defun cal-korea-x-solar-term-alist-new (year)
  "Return a solar-term alist for YEAR."
  (loop for i from 0 upto 23

        for date = (cal-korea-x-next-solar-term `(1 1 ,year))
        then (setq date (cal-korea-x-next-solar-term date))

        with solar-term-alist = '()

        collect (cons date (aref cal-korea-x-solar-term-name i))
        into solar-term-alist

        finally return solar-term-alist))

;; cached solar terms in a year
(defvar cal-korea-x-solar-term-alist nil) ; e.g., '(((9 23 2013) "추분") ...)
(defvar cal-korea-x-solar-term-year nil)

(defun cal-korea-x-sync-solar-term (year)
  "Sync `cal-korea-x-solar-term-alist' and `cal-korea-x-solar-term-year' to YEAR."
  (unless (and cal-korea-x-solar-term-year
               (= cal-korea-x-solar-term-year year))
      (setq cal-korea-x-solar-term-alist
            (cal-korea-x-solar-term-alist-new year))
      (setq cal-korea-x-solar-term-year
            (extract-calendar-year
             (caar cal-korea-x-solar-term-alist)))))

(defun holiday-solar-term-ko (solar-term str)
  "A holiday(STR) on SOLAR-TERM day.
See `cal-korea-x-solar-term-name' for a list of solar term names ."
  (cal-korea-x-sync-solar-term displayed-year)
  (let ((l cal-korea-x-solar-term-alist)
        date)
    (dolist (i l)
      (when (string= (cdr i) solar-term)
        (setq l '()
              date (car i))))
    (holiday-fixed (car date) (cadr date) str)))

;; =============================================================================
;; for diary lunar item

(defvar diary-korean-date-forms
  '((month "[-/]" day "[^-/0-9]")
    (year "[-/]" month "[-/]" day "[^0-9]")
    (month "월 " day "일" "[^0-9]")
    (year "년 " month "월 " day "일" "[^0-9]")
    (dayname "\\W"))
  "*List of pseudo-patterns describing the Korean patterns of date used.
See the documentation of `diary-date-forms' for an explanation.")

(setq diary-date-forms diary-korean-date-forms)

(setq diary-modify-entry-list-string-function
       'calendar-modify-diary-entry-string)

(defun diary-lunar-date (month day year &optional leap mark)
  "Specific date(s) diary entry."
  (let* (
         (l-leap (if leap " (윤)" ""))
         (datestr (format "%d월 %d일%s" month day l-leap))
         (c-l-date (lunar-ko-lunar-date (or date (calendar-current-date))))
         (c-l-y (lunar-ko-year c-l-date))
         (c-l-m (lunar-ko-month c-l-date))
         (c-l-d (lunar-ko-day c-l-date))
         (c-l-leap (lunar-ko-leap? c-l-date))
         )
    (if (and
         (equal year c-l-y)
         (equal month c-l-m)
         (equal day c-l-d)
         (equal leap c-l-leap)
         )
        (cons mark (format (replace-regexp-in-string "[\t ]+" "" entry) (format "음력 %s" datestr))))))

(defun diary-lunar-anniversary (month day &optional leap mark)
  "Specific date(s) diary entry."
  (let* (
         (l-leap (if leap " (윤)" ""))
         (datestr (format "%d월 %d일%s" month day l-leap))
         (c-l-date (lunar-ko-lunar-date (or date (calendar-current-date))))
         (c-l-m (lunar-ko-month c-l-date))
         (c-l-d (lunar-ko-day c-l-date))
         (c-l-leap (lunar-ko-leap? c-l-date))
         )
    (if (and
         (equal month c-l-m)
         (equal day c-l-d)
         (equal leap c-l-leap)
         )
        (cons mark (format (replace-regexp-in-string "[\t ]+" "" entry) (format "음력 %s" datestr))))))

(defun diary-solar-24term (solar-term &optional mark)
  "A holiday(STR) on SOLAR-TERM day."
  (let* (
         (c-date (or date (calendar-current-date)))
         (y (extract-calendar-year c-date))
         (m (extract-calendar-month c-date))
         (d (extract-calendar-day c-date)))
    (cal-korea-x-sync-solar-term y)
    (let ((l cal-korea-x-solar-term-alist)
          date)
      (dolist (i l)
        (when (string= (cdr i) solar-term)
          (setq l '()
                date (car i))))
      (let (
            (s-m (car date))
            (s-d (cadr date)))
        (if (and
             (equal s-m m)
             (equal s-d d))
            (cons mark (replace-regexp-in-string "[\t ]+" "" entry)))))))

(provide 'cal-korea-x)
;;; cal-korea-x.el ends here
