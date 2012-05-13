cal-korea-x
===========

This is Korean-localized extras for GNU Emacs calendar, inspired by [cal-china-x](http://xwl.appspot.com/ref/cal-china-x.el).

This package provides following features:

* Korean holidays
* Korean Month names (e.g. "2012년 5월" instead "May 2012")
* Convering Gregorian date into Korean lunar date (`p K` in Calendar mode)
* Goto Gregorian date from Korean lunar date (`g K` in Calendar mode)


Why don't use Chinese lunar Calendar?
-------------------------------------

Mostly, the Korean lunar calenar is almost the same as Chinese one. However, there are few difference since the standard meridian of two countries are different.

Implementation Consideration
----------------------------
There are a few known formula to convert the Gregorian and Korean lunar date and vice vesa.  However, according to [Korean Astronomy & Space Science Information](http://astro.kasi.re.kr/), there is no perfect formular for the conversion.

That's why this package uses pre-calculated table from the institution.

 


