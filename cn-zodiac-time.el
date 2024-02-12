;;; cn-zodiac-time.el --- Format time using Chinese zodiac. -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Cameron Desautels

;; Author: Cameron Desautels <camdez@gmail.com>
;; Version: 0.1
;; Homepage: http://github.com/camdez/cn-zodiac-time.el
;; Keywords: calendar, games

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library renders times via emoji / Chinese characters of the
;; zodiac animals, primarily for displaying the current time on the
;; Emacs mode-line.
;;
;; It's a toy, not a serious calendar implementation—if that wasn't
;; clear from the cartoon animals. 🦁
;;
;; The simplest way to use the library is by calling `cn-zodiac-time',
;; which renders the current time according to predefined styles.
;;
;; The following two invocations are equivalent:
;;
;;     (cn-zodiac-time)
;;     (cn-zodiac-time 'cute)
;;
;; ==> "小🐯 一分"
;;
;; For full documentation, see: https://github.com/camdez/cn-zodiac-time.el

;;; Code:

(defconst cn-zodiac-time-emoji
  ["🐀" "🐄" "🐅" "🐇" "🐉" "🐍" "🐎" "🐑" "🐒" "🐓" "🐕" "🐖"])

(defconst cn-zodiac-time-cute-emoji
  ["🐭" "🐮" "🐯" "🐰" "🐲" "🐍" "🐴" "🐏" "🐵" "🐔" "🐶" "🐷"])

(defconst cn-zodiac-time-earthly-branches
  ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])

(defconst cn-zodiac-time-traditional
  ["鼠" "牛" "虎" "兔" "龍" "蛇" "馬" "羊" "猴" "雞" "狗" "豬"])

(defconst cn-zodiac-time-simplified
  ["鼠" "牛" "虎" "兔" "龙" "蛇" "马" "羊" "猴" "鸡" "狗" "猪"])

(defconst cn-zodiac-time-size-indicators '(("小") ("大")))
(defconst cn-zodiac-time-traditional-indicators '((nil "初") (nil "正")))

(defconst cn-zodiac-time-digits
  [nil "一" "二" "三" "四" "五" "六" "七" "八" "九"])

(defun cn-zodiac-time--format-minutes (mins &optional suffix)
  "Format MINS minutes (1-59) using Chinese characters.

Optional SUFFIX defaults to \"分\".  Pass empty string to remove."
  (let ((suffix (or suffix "分"))
        (tens (/ mins 10))
        (ones (mod mins 10)))
    (concat
     (when (> tens 1)
       (elt cn-zodiac-time-digits tens))
     (when (> tens 0)
       "十")
     (when (> ones 0)
       (elt cn-zodiac-time-digits ones))
     suffix)))

(defun cn-zodiac-time--format-shi (shi half shi-strs half-indics)
  (let ((hi (elt half-indics half)))
    (concat (elt hi 0) (elt shi-strs shi) (elt hi 1))))

(defun cn-zodiac-format-time (time shi-strs half-indics &optional hide-mins mins-suffix)
  (let* ((dtime  (decode-time time))
         (hrs    (decoded-time-hour dtime))
         (mins   (decoded-time-minute dtime))
         (n      (mod (1+ hrs) 24))
         (half   (mod n 2))
         (shi    (/ n 2))
         (shi-pt (cn-zodiac-time--format-shi shi half shi-strs half-indics)))
    (if (or hide-mins (zerop mins))
        shi-pt
      (concat shi-pt " " (cn-zodiac-time--format-minutes mins mins-suffix)))))

(defun cn-zodiac-time (&optional style hide-mins time)
  "Format TIME in predefined STYLE using emoji or Chinese characters.

STYLE should be one of the following symbols, or nil for the
default style: branches, cute, emoji, simplified, traditional.

To display only the hour (without minutes), provide a truthy
value for HIDE-MINS."
  (let* ((time        (or time (current-time)))
         (style       (or style 'cute))
         (shi-strs    (pcase style
                        ('branches    cn-zodiac-time-earthly-branches)
                        ('cute        cn-zodiac-time-cute-emoji)
                        ('emoji       cn-zodiac-time-emoji)
                        ('simplified  cn-zodiac-time-simplified)
                        ('traditional cn-zodiac-time-traditional)
                        (_            (error "Invalid style: %S" style))))
         (half-indics (if (eq 'branches style)
                          cn-zodiac-time-traditional-indicators
                        cn-zodiac-time-size-indicators)))
    (cn-zodiac-format-time time shi-strs half-indics hide-mins)))

(provide 'cn-zodiac-time)
;;; cn-zodiac-time.el ends here
