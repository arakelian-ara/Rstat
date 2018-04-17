---------++++++++++++
proleptic Gregorian
The proleptic Gregorian calendar is produced by extending the Gregorian calendar backward to dates preceding its official introduction in 1582

---------++++++++++++
We recommend that the unit year be used with caution. The Udunits package defines a year to be exactly 365.242198781 days (the interval between 2 successive passages of the sun through vernal equinox). It is not a calendar year. Udunits includes the following definitions for years: a common_year is 365 days, a leap_yearis 366 days, a Julian_year is 365.25 days, and aGregorian_year is 365.2425 days.

For similar reasons the unit month , which is defined in udunits.dat to be exactly year/12 , should also be used with caution.

The mixed Gregorian/Julian calendar used by Udunits is explained in the following excerpt from the udunits(3) man page:

The udunits(3) package uses a mixed Gregorian/Julian calen- dar system. Dates prior to 1582-10-15 are assumed to use the Julian calendar, which was introduced by Julius Caesar in 46 BCE and is based on a year that is exactly 365.25 days long. Dates on and after 1582-10-15 are assumed to use the Gregorian calendar, which was introduced on that date and is based on a year that is exactly 365.2425 days long. (A year is actually approximately 365.242198781 days long.) Seem- ingly strange behavior of the udunits(3) package can result if a user-given time interval includes the changeover date. For example, utCalendar() and utInvCalendar() can be used to show that 1582-10-15 *preceded* 1582-10-14 by 9 days.

The rule for leap years was changed. In the Julian Calendar a year is a leap year if it is divisible by 4. In the Gregorian Calendar a year is a leap year if either (i) it is divisible by 4 but not by 100 or (ii) it is divisible by 400. In other words, a year which is divisible by 4 is a leap year unless it is divisible by 100 but not by 400 (in which case it is not a leap year). Thus the years 1600 and 2000 are leap years, but 1700, 1800, 1900 and 2100 are not.

---------++++++++++++



double time(time) ;
> time:units = "hours since 1800-1-1 00:00:0.0" ;
> time:long_name = "time" ;
> time:actual_range = 1750224., 1750941. ;
> time:delta_t = "0000-00-00 03:00:00" ;
> time:standard_name = "time" ;
> time:axis = "T" ;


---------++++++++++++

If actual_range exists
Convert into days
Read/parse Origin
read/parse Origin_time
 À quoi correspond time_value[1]
 ‎comment évolue l'increment en time_value[2] - time_value[1]
 ‎pas de temps entier ? On aura besoin du delta_t !
 ‎unité de l'axe ? Rien à faire !





ncdf                   package:ncdf                    R Documentation
This is version 1.6 of the ncdf library.
David W. Pierce <email: dpierce@ucsd.edu>
'att.put.ncdf', 'att.get.ncdf', 'close.ncdf', 'create.ncdf',
     'dim.def.ncdf', 'get.var.ncdf', 'put.var.ncdf', 'open.ncdf',
     'print.ncdf', 'set.missval.ncdf', 'sync.ncdf', 'var.def.ncdf'.
     'redef.ncdf'.

