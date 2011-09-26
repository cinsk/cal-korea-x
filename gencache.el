;;; gencache.el --- 

;; Copyright (C) 2011  Seong-Kook Shin

;; Author: Seong-Kook Shin <cinsky@gmail.com>
;; Keywords: 

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

(require 'calendar)

;;
;;emacs -L . -l gencache --batch --eval '(lunar-ko-generate-cache "./script/cache.dat" "cache.el"))'
;;

;
; TODO: consult `batch-byte-compile' and modify
; `lunar-ko-generate-cache' so that it can get the arguments from
; command line.
;

(defun gregorian-to-julian (date)
  "Convert gregorian date in the form of (MONTH DAY YEAR) to
Julian day number.  See also `calendar-astro-date-string'."
  (calendar-astro-from-absolute
   (calendar-absolute-from-gregorian date)))

(defun lunar-ko-generate-cache (srcfile &optional outfile)
  (let ((coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    (let* ((srcpath (expand-file-name srcfile))
           (outpath (expand-file-name 
                     (or outfile
                         (replace-regexp-in-string "\\.[^.]*$" ".el" srcfile)))))
      (message "srcpath: %s" srcpath)
      (message "outpath: %s" outpath)
      (message "args: %S" command-line-args-left)
      (let ((srcbuf (find-file-noselect srcpath t t))
            (outbuf (create-file-buffer outpath)))
        (save-current-buffer
          (set-buffer outbuf)
          (setq buffer-file-name outpath)
          ;;(setq buffer-file-coding-system 'utf-8)
          (emacs-lisp-mode))
        (save-current-buffer
          (set-buffer srcbuf)
          (goto-char (point-min))
          (while (search-forward-regexp
                  "^\\([0-9]*\\)-\\([0-9]*\\)-\\([0-9]*\\) \\([0-9]*\\) \\(.*\\)$"
                  nil t)
            (let ((year (string-to-number (match-string-no-properties 1)))
                  (month (string-to-number (match-string-no-properties 2)))
                  (day (string-to-number (match-string-no-properties 3)))
                  (lunar (match-string-no-properties 4))
                  (text (match-string-no-properties 5)))
              (save-current-buffer
                (set-buffer outbuf)
                (goto-char (point-max))
                (let ((date (list month day year)))
                  (indent-according-to-mode)
                  (insert (format "(%.1f . %s)  ; %s\n"
                                  (gregorian-to-julian date)
                                  lunar text)))))))
        (save-current-buffer
          (set-buffer outbuf)
          (save-buffer))
        (kill-buffer srcbuf)
        (kill-buffer outbuf)))))



(provide 'gencache)
;;; gencache.el ends here
