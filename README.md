cal-korea-x
===========

This is Korean-localized extras for GNU Emacs calendar, inspired by [cal-china-x](http://xwl.appspot.com/ref/cal-china-x.el).

This package provides following features:

* Korean holidays
* Korean Month names (e.g. "2012년 5월" instead "May 2012")
* Convering Gregorian date into Korean lunar date.
* Goto Gregorian date from Korean lunar date.


Why don't use Chinese lunar Calendar?
-------------------------------------

Mostly, the Korean lunar calenar is almost the same as Chinese one. However, there are few difference since the standard meridian of two countries are different.

Installation
------------
1. Download the source and put somewhere you want to.  I sugggest to install in `$HOME/.emacs.d/cal-korea-x`.

        $ git clone http://github.com/cinsk/cal-korea-x.git $HOME/.emacs.d/cal-korea-x

2. Add following code in your init file (`.emacs` or `.emacs.d/init.el`)

        (add-to-list `load-path (expand-file-name "~/.emacs.d/cal-korea-x"))
        (require 'cal-korea-x)
        (setq holiday-general-holidays cal-korea-x-korean-holidays)

Usage
-----
After installation, `M-x calendar` automatically uses Korean display name for calendar.

* `p K` displays the Korean lunar date for the current date.
* `g K` accepts the Korean lunar date and goto the corresponding Gregorian date.

Implementation Consideration
----------------------------
There are a few known formula to convert the Gregorian and Korean lunar date and vice vesa.  However, according to [Korean Astronomy & Space Science Information](http://astro.kasi.re.kr/), there is no perfect formular for the conversion.

That's why this package uses pre-calculated table from the institution.

 


