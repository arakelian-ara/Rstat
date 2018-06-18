###############################################################################
#  --------- Netcdf Warpper of Time handler -----------------------------------
#  Function to read time axis of a netcdf file
#  This Function is a kind of wrapper of the time_handler functio (see below)
#  Return a list with lot of informations, usable for sorting / selection (which)
#  and readable by a human
# -----------------------------------------------------------------------------
nc_time_handler <- function(filename,ta_variable_name='time',sub_daily_info='auto',overwrite_calendar=FALSE,overwrite_units=FALSE,overwrite_time=FALSE){
# -----------------------------------------------------------------------------
# example :
# Easy to use : my_time=nc_time_handler('filename.nc','time');
# Complete option use : my_time=nc_time_handler('filename.nc','time',sub_daily_use=TRUE);
# Not advise use : my_time=nc_time_handler('filename.nc','time',TRUE,'gregorian',"days since 1950-01-01 00:00:00",seq(0,10,.25));
# -----------------------------------------------------------------------------
# INPUT :
# filename                  = character : path+file name (netcdf)
# ta_variable_name='time'   = character : default 'time', change if time axis axe an other name
# sub_daily_info='auto'     = default : automatic define if sub-daily information are needed
#                           = TRUE : force to add "00:00:00" for output for each time step
#                           = FALSE : force to remove sub-daily information for each time step
# overwrite_XXX             = allow to overwrite informations from netcdf file, USE WITH CAUTION
# overwrite_calendar=FALSE  = FALSE : read inforamtion from netcdf file
#                           = character : replace information from netcdf file with specified character among
#                                         'gregorian','noleap','all_leap','360_day','julian','none'
# overwrite_units=FALSE     = FALSE : read inforamtion from netcdf file
#                           = character : replace information from netcdf file with specific synthax
#                                         as "unit since yyyy-mm-dd HH:MM:SS"
# overwrite_time=FALSE      = FALSE : read inforamtion from netcdf file
#                           = numeric : replace time values with your own value, must be a vector of numeric
# -----------------------------------------------------------------------------
# OUTPUT :
# A list whose number of named elements depends on the presence of sud-daily informations
# With or without sub-daily data
# list$calendar = character among 'gregorian','noleap','all_leap','360_day','julian','none'
# list$unit     = character among 'days','hours','seconds'
# list$origin   = character as yyyy:mm:dd HH:MM:SS
# list$ymd      = numeric as yyyymmdd (year_month_day)
# list$yyyy     = numeric : year
# list$mm       = numeric : month of the year [1~12]
# list$dd       = numeric : day of the month [1~31]
# list$YYYYMMDD = charcter as "yyyy-mm-dd"
# If there is some sud-daily information, list contains more informations :
# list$HH       = numeric : hour [0~23]
# list$MM       = numeric : minutes [0~59]
# list$SS       = numeric : seconds [0~59]
# list$SSM      = numeric : seconds since midnight
# list$HHMMSS   = character as "HH:MM:DD"
# -----------------------------------------------------------------------------
# Notes :
# You can obtain the origin attribut of netcdf file with : paste(list$unit,'since',list$origin,sep=" ")
# -----------------------------------------------------------------------------
  # check if necdf4 package in installed
  if ('ncdf4' %in% installed.packages()){
    library('ncdf4');
    nc=nc_open(filename,write=FALSE);
    TIME=as.vector(ncvar_get(nc,ta_variable_name));
    TIME_AXIS_ATT=ncatt_get(nc,ta_variable_name);
    nc_close(nc);
  } else if('RNetCDF' %in% installed.packages()){
    library('RNetCDF');
    nc=open.nc(filename,write=FALSE);
    TIME=as.vector(var.get.nc(nc,ta_variable_name));
    if (is.character(att.get.nc(nc,ta_variable_name,'units'))){
      TIME_AXIS_ATT$units=att.get.nc(nc,ta_variable_name,'units');
    }
    if (is.character(att.get.nc(nc,ta_variable_name,'calendar'))){
      TIME_AXIS_ATT$units=att.get.nc(nc,ta_variable_name,'calendar');
    }
  } else if('ncdf' %in% installed.packages()){
    stop("Package ‘ncdf’ was removed from the CRAN repository. Archived on 2016-01-11: use 'RNetCDF' or 'ncdf4' instead.");
  } else {
    stop('ncdf4 or RNetCDF package must be installed')
  }
  # This two line are planned to be used for test, use carefuuly
  if (is.character(overwrite_calendar)){TIME_AXIS_ATT$calendar=overwrite_calendar;}
  if (is.character(overwrite_units)){TIME_AXIS_ATT$units=overwrite_units;}
  if (is.numeric(overwrite_time)){TIME=overwrite_time;}

  if (('calendar' %in% tolower(names(TIME_AXIS_ATT)))==FALSE){
    warning(cat('warning : \'calendar\' is missing as \'',ta_variable_name,'\' attribut','\n',
                'warning : \'calendar\' is now define as gregorian ie standard\n',sep=""),call.=FALSE,immediate.=TRUE);
    TIME_AXIS_ATT$calendar='gregorian';
  }
  if (('units' %in% tolower(names(TIME_AXIS_ATT)))==FALSE){
    warning(cat('warning : \'units\' is missing as \'',ta_variable_name,'\' attribut','\n',
                'warning : \'calendar\' is now define as ',TIME_AXIS_ATT$units,'\n',sep=""),call.=FALSE,immediate.=TRUE);
    TIME_AXIS_ATT$units="days since 1900-01-01 00:00:00"
  }
  if (is.numeric(TIME)==FALSE){
    stop(paste0('It\'s look like your variable \'',ta_variable_name,'\' is not a numeric vector'));
  }
  if (calendar=='none'){TIME=1;} # none calendar specific case
  return(time_handler(TIME,TIME_AXIS_ATT$calendar,TIME_AXIS_ATT$units,sub_daily_info=sub_daily_info));
}
###############################################################################
###############################################################################
#  --------- Time handler -----------------------------------------------------
#  Function to transform time values consistent with calendar and unit
#  Return a list with lot of informations, usable for sorting / selection (which)
#  and readable by a human
# -----------------------------------------------------------------------------
time_handler <- function(time_value,calendar,units,sub_daily_info='auto'){
# -----------------------------------------------------------------------------
# example :
# Easy to use : my_time=nc_time_handler('filename.nc','time');
# Complete option use : my_time=nc_time_handler('filename.nc','time',sub_daily_use=TRUE);
# Not advise use : my_time=nc_time_handler('filename.nc','time',TRUE,'gregorian',"days since 1950-01-01 00:00:00",seq(0,10,.25));
# -----------------------------------------------------------------------------
# INPUT :
# time_value                = numeric : time values consistent with calendar and units
# calendar                  = character : among 'gregorian','noleap','all_leap','360_day','julian','none'
# units                     = character : "unit since yyyy-mm-dd HH:MM:SS"
#                                         with unit among 'days','hours','seconds'
# sub_daily_info='auto'     = default : automatic define if sub-daily information are needed
#                           = TRUE : force to add "00:00:00" for output for each time step
#                           = FALSE : force to remove sub-daily information for each time step
# -----------------------------------------------------------------------------
# OUTPUT :
# A list whose number of named elements depends on the presence of sud-daily informations
# With or without sub-daily data
# list$calendar = character among 'gregorian','noleap','all_leap','360_day','julian','none'
# list$unit     = character among 'days','hours','seconds'
# list$origin   = character as yyyy:mm:dd HH:MM:SS
# list$ymd      = numeric as yyyymmdd (year_month_day)
# list$yyyy     = numeric : year
# list$mm       = numeric : month of the year [1~12]
# list$dd       = numeric : day of the month [1~31]
# list$YYYYMMDD = charcter as "yyyy-mm-dd"
# If there is some sud-daily information, list contains more informations :
# list$HH       = numeric : hour [0~23]
# list$MM       = numeric : minutes [0~59]
# list$SS       = numeric : seconds [0~59]
# list$SSM      = numeric : seconds since midnight
# list$HHMMSS   = character as "HH:MM:DD"
# -----------------------------------------------------------------------------
# Notes :
# You can obtain the origin attribut of netcdf file with : paste(list$unit,'since',list$origin,sep=" ")
# -----------------------------------------------------------------------------
  calendar=tolower(calendar); # everything in lower case to easier comparaison
  units=tolower(units); # everything in lower case to easier comparaison
  Unit=strsplit(units," ")[[1]][1];
  Origin=strsplit(units," ")[[1]][3];
  Yb=strsplit(Origin,'-')[[1]][1];
  #Mb=strsplit(Origin,'-')[[1]][2];
  #Db=strsplit(Origin,'-')[[1]][3];
  if (calendar=='none'){time_value=1;} # none calendar specific case
  if (length(strsplit(units," ")[[1]])==4){
    origin_sub_daily_info=TRUE;
    origin_time=strsplit(units," ")[[1]][4];
    if (origin_time=="00:00:0.0"){origin_time="00:00:00";}
    if (all(as.numeric(strsplit(origin_time,':')[[1]])==0)){
      # origin_time is at midnight - no shift need
      time_axis_sub_daily_need=FALSE;
    } else if (any(as.numeric(strsplit(origin_time,':')[[1]])!=0)){
      # origin_time is NOT midnight - shift NEEDED
      time_axis_sub_daily_need=TRUE;
    }
  } else if (length(strsplit(units," ")[[1]])==3){
    time_axis_sub_daily_need=FALSE;
    origin_sub_daily_info=FALSE;
  }
  # standardization of time calendar
  calendar=calendar_handling(calendar);
  # standardization of time axis
  Unit=unit_handling(Unit);
  # convert unit into days either way
  if (Unit!='days'){
    if (Unit=='hours'){
      time_value=time_value/24;
    } else if (Unit =='minutes'){
      time_value=time_value/(24*60)
    } else if (Unit =='seconds'){
      time_value=time_value/(24*60*60)
    }
    Unit='days'
  }
  # If origin_time is NOT midnight - shift NEEDED
  if (time_axis_sub_daily_need==TRUE){
    time_value=time_value*24*60*60; # time_value are in days - convert to second for shift
    # shift formula depends on origin_time form/synthax
    if (length(strsplit(origin_time,':')[[1]])==1){
      time_value=time_value+as.numeric(strsplit(origin_time,':')[[1]][1])*60*60;
    } else if (length(strsplit(origin_time,':')[[1]])==2){
      time_value=time_value+as.numeric(strsplit(origin_time,':')[[1]][1])*60*60+as.numeric(strsplit(origin_time,':')[[1]][2])*60;
    } else if (length(strsplit(origin_time,':')[[1]])==3){
      time_value=time_value+as.numeric(strsplit(origin_time,':')[[1]][1])*60*60+as.numeric(strsplit(origin_time,':')[[1]][2])*60+as.numeric(strsplit(origin_time,':')[[1]][3]);
    } else {
      stop('unknow sub daily information at origin synthax')
    }
    time_value=time_value/(24*60*60); # time_value return in days after shift
  }
  # If there is sub daily data return 1 (There Is Sub-daily Data)
  tisd=there_is_sub_daily(time_value);
  # Sub-daily informations transformation
  hhmmss=hhmmss_conversion(tisd,time_value);
  # Sub-daily informations transformation into readable by humans
  hhmmss_print=hhmmss_printer(tisd,hhmmss);

  # Sub Daily information trigger for output
  if (sub_daily_info=='auto'){
    if (tisd==TRUE){
      sub_daily_info=TRUE;
    }
    if (tisd==FALSE){
        sub_daily_info=FALSE;
    }
  }
  # Sub-daily informations are removed from time_value
  time_value_day=floor(time_value);
  if (calendar %in% c('none')){
    time_value_day=1;
    yymmdd=yymmdd_conversion(calendar,time_value_day,Origin,Yb);
  } else if (calendar %in% c('none','proleptic_gregorian','gregorian','noleap','all_leap','360_day','julian')){
    yymmdd=yymmdd_conversion(calendar,time_value_day,Origin,Yb);
  } else {
  error('unkown calendar not yet');
  }
  yymmdd_print=yymmdd_printer(yymmdd);


 if (sub_daily_info==TRUE & length(hhmmss_print)==1){
    hhmmss=rep(hhmmss,length(time_value));
    hhmmss_print=rep(hhmmss_print,length(time_value));
  }
  # Returned list
  if(sub_daily_info==TRUE){
    if (exists("origin_time")!=TRUE){origin_time="00:00:00";}
    Origin=paste(Origin,origin_time,sep=" ");
    return(list(calendar=calendar,unit=Unit,origin=Origin,
           ymd=yymmdd$YMD,yyyy=yymmdd$YYYY,mm=yymmdd$MM,dd=yymmdd$DD,YYYYMMDD=yymmdd_print,
           HH=hhmmss$hh,MM=hhmmss$mm,SS=hhmmss$ss,SSM=hhmmss$ssm,HHMMSS=hhmmss_print));
  } else if (sub_daily_info==FALSE){
    return(list(calendar=calendar,unit=Unit,origin=Origin,
           ymd=yymmdd$YMD,yyyy=yymmdd$YYYY,mm=yymmdd$MM,dd=yymmdd$DD,YYYYMMDD=yymmdd_print));
  }
}
###############################################################################
###############################################################################
#  --------- Printer ----------------------------------------------------------
#  Simple function to print the output of time_handler or nc_time_handler
#  Look like a cdo sinfon file.nc
# -----------------------------------------------------------------------------
time_handler_printer <- function(TT){
# -----------------------------------------------------------------------------
# Example :
# To compare if reading with R is the same than cdo sinfon command:
if (FALSE){
   system(paste0('cdo sinfo ',file,'|tail -n36'));
   print('-------------------------------------------------------------------------------');
   print('-------------------------------------------------------------------------------');
   TT=nc_time_handler(file,'time',TRUE);
   time_handler_printer(TT);
}
# -----------------------------------------------------------------------------
# INPUT :
# TT : list : output To Test (TT) of time_handler or nc_time_handler function
# -----------------------------------------------------------------------------
# NO OUTPUT
# -----------------------------------------------------------------------------
  print(paste(TT$calendar,TT$unit,TT$origin,sep=' || '));
  L=length(TT$ymd);
  print(paste('Length of time axis :',L,'steps',sep=" "));
  LL=4;
  T1=c(1:(5*LL));T1=seq(T1[1],T1[length(T1)],LL);
  T2=c((L-(5*LL-1)):L);T2=seq(T2[1],T2[length(T2)],LL);
  if (length(TT)==8){
    for(t in T1){
      print(paste(TT$YYYYMMDD[t],TT$YYYYMMDD[t+1],TT$YYYYMMDD[t+2],TT$YYYYMMDD[t+3],sep="  "));
    }
    print('..............................................');
    print('..............................................');
    for(t in T2){
      print(paste(TT$YYYYMMDD[t],TT$YYYYMMDD[t+1],TT$YYYYMMDD[t+2],TT$YYYYMMDD[t+3],sep="  "));
    }
  }

  if (length(TT)==13){
    print('sub daily info are in the output');
    for(t in T1){
      print(paste(paste(TT$YYYYMMDD[t],TT$HHMMSS[t],sep=" "),paste(TT$YYYYMMDD[t+1],TT$HHMMSS[t+1],sep=" "),
                  paste(TT$YYYYMMDD[t+2],TT$HHMMSS[t+2],sep=" "),paste(TT$YYYYMMDD[t+3],TT$HHMMSS[t+3],sep=" "),sep="  "));
    }
    print('..............................................');
    print('..............................................');
    for(t in T2){
      print(paste(paste(TT$YYYYMMDD[t],TT$HHMMSS[t],sep=" "),paste(TT$YYYYMMDD[t+1],TT$HHMMSS[t+1],sep=" "),
                  paste(TT$YYYYMMDD[t+2],TT$HHMMSS[t+2],sep=" "),paste(TT$YYYYMMDD[t+3],TT$HHMMSS[t+3],sep=" "),sep="  "));

    }
  }
}
###############################################################################
###############################################################################
#  --------- Calendar handling ------------------------------------------------
#  To homogenize the calendar type
# -----------------------------------------------------------------------------
calendar_handling <- function(calendar) {
# -----------------------------------------------------------------------------
# INPUT :
# calendar = character
# -----------------------------------------------------------------------------
# OUTPUT :
# calendar = character : among 'gregorian','noleap','all_leap','360_day','julian','none'
# -----------------------------------------------------------------------------
         if (calendar %in% c('gregorian','standard')){
  # Mixed Gregorian/Julian calendar as defined by Udunits. This is the default.
    calendar='gregorian';
  } else if (calendar %in% c('proleptic_gregorian')){
  # A Gregorian calendar extended to dates before 1582-10-15. That is, a year is a leap year if either (i) it is divisible by 4 but not by 100 or (ii) it is divisible by 400.
  # NEEDED ADD NOTE
    calendar='gregorian';
  } else if (calendar %in% c('noleap','365_day','365day','365d')){
  #Gregorian calendar without leap years, i.e., all years are 365 days long.
    calendar='noleap';
  } else if (calendar %in% c('all_leap','366_day','366day','366d')){
  # Gregorian calendar with every year being a leap year, i.e., all years are 366 days long.
    calendar='all_leap';
  } else if (calendar %in% c('360_day','360day','360d')){
  # All years are 360 days divided into 30 day months.
    calendar='360_day';
  } else if (calendar %in% c('julian')){
  #Julian calendar
    calendar='julian';
  } else if (calendar %in% c('none')){
  # The calendar attribute may be set to none in climate experiments that simulate a fixed time of year.
  calendar='none';
  } else {
    stop(' This type of calendar is not supported \n Maybe it is just the attribut of netcdf file that is not supported, i.e., string comparaison');
  }
  return(calendar)
}
###############################################################################
###############################################################################
#  --------- Unit handling ----------------------------------------------------
#  To homogenize the unit type
# -----------------------------------------------------------------------------
unit_handling <- function(unit) {
# -----------------------------------------------------------------------------
# INPUT :
# unit = character
# -----------------------------------------------------------------------------
# OUTPUT :
# unit = character among 'days','hours','seconds'
# -----------------------------------------------------------------------------
         if (unit %in% c('days')){
    unit='days';
  } else if (unit %in% c('hours')){
    unit='hours';
  } else if (unit %in% c('seconds')){
    unit='seconds';
  } else {
  stop(' This type of unit is not supported \n Maybe it is just the attribut of netcdf file that is not supported, i.e., string comparaison')
  }
  return(unit)
}
###############################################################################
###############################################################################
#  --------- Time Axis : Daily or more ? or sub-daily ? -----------------------
#  Test all time values (in days) to find out if there's at least one value is
#  not an integer, ie sub-daily, steps
#  if there is sub daily data return 1
#  if not return 0
#  WARNING: this test concerns values expressed in days
# -----------------------------------------------------------------------------
there_is_sub_daily <- function(time_value) {
# -----------------------------------------------------------------------------
# INPUT :
# time_value  = numeric : time values (in days)
# -----------------------------------------------------------------------------
# OUTPUT :
# 1 = with sub-daily data
# 0 = without sub-daily data
# -----------------------------------------------------------------------------
  if (all(time_value%%1==0)){
    return(0); # If all value of time_value are integer NO NEED of HH:MM:SS
  } else {
    return(1); # HH:MM:SS is NEEDED
  }
}
###############################################################################
###############################################################################
#  --------- Convertion of sub-daily values as HH:MM:SS -----------------------
#  Convertion of non integer part of time_value (expressed in days) into HH:MM:SS
#  This calculation does not depend on the type of calendar
# -----------------------------------------------------------------------------
hhmmss_conversion <- function(tisd,time_value) {
# -----------------------------------------------------------------------------
# INPUT :
# tisd        = numeric : 1 or 0 : with or without sub-daily data
# time_value  = numeric : time values (in days)
# -----------------------------------------------------------------------------
# OUTPUT :
# A list with
# list$hh  = numeric : hour [0~23]
# list$mm  = numeric : minutes [0~59]
# list$ss  = numeric : seconds [0~59]
# list$ssm = numeric : seconds since midnight
# -----------------------------------------------------------------------------
  if (tisd==0){
    return(list(hh=0,mm=0,ss=0,ssm=0));
  }
  if (tisd==1){
    subdaily_value=time_value%%1;
    hh=floor(subdaily_value*24);
    mm=floor(((subdaily_value*24)-hh)*60);
    ss=floor(((((subdaily_value*24)-hh)*60)-mm)*60);
    ms=((((((subdaily_value*24)-hh)*60)-mm)*60)%%1);
    if(any(ms!=0)){
      ss=ss+ms;
    }
    ssm=subdaily_value*24*60*60; # seconds since midnight
    return(list(hh=hh,mm=mm,ss=ss,ssm=ssm));
  }
}
###############################################################################
###############################################################################
#  --------- Human readable HH:MM:SS ------------------------------------------
#  Create human readable HH:MM:SS (hours:minutes:seconds)
#  Seconds part can be integer or float with 3 digit only
# -----------------------------------------------------------------------------
hhmmss_printer <- function(tisd,hhmmss){
# -----------------------------------------------------------------------------
# INPUT :
# tisd    = numeric : 1 or 0 : with or without sub-daily data
# hhmmss  = list as hhmmss_conversion return
# -----------------------------------------------------------------------------
# OUTPUT :
# character : for each time steps sub-daily informations as 'HH:MM:SS'
#             or 'HH:MM:S.S' if there is some under seconds informations
# -----------------------------------------------------------------------------
  if (tisd==0){
    return("00:00:00");
  }
  if (tisd==1){
    if (all(hhmmss$ss%%1==0)){
      return(apply(cbind(hhmmss$hh,hhmmss$mm,hhmmss$ss),1,function(x){sprintf("%02d:%02d:%02d",x[1],x[2],x[3])}));
    } else {
      return(apply(cbind(hhmmss$hh,hhmmss$mm,hhmmss$ss),1,function(x){sprintf("%02d:%02d:%06.3f",x[1],x[2],x[3])}));
    }
  }
}
###############################################################################
###############################################################################
#  --------- Convertion of Days values as HH:MM:SS -----------------------
#  Convertion of integer part of time_value (expressed in days) into yyyy-mm-dd
#  This calculation depends on the type of calendar
# -----------------------------------------------------------------------------
yymmdd_conversion <- function(calendar,time_day,date_origin,year_begin){
# -----------------------------------------------------------------------------
# INPUT :
# calendar    = character (homogenized)
# time_day    = numeric integer part of time value (in days)
# date_origin = character as "yyyy-mm-dd"
# year_begin  = character as "yyyy"
# -----------------------------------------------------------------------------
# OUTPUT :
# A list with
# list$YMD      = numeric as yyyymmdd (year_month_day)
# list$YYYY     = numeric : year
# list$MM       = numeric : month of the year [1~12]
# list$DD       = numeric : day of the month [1~31]
# -----------------------------------------------------------------------------
  if (calendar %in% c('none')){
    DD=strtoi(strsplit(date_origin,'-')[[1]][3]);
    MM=strtoi(strsplit(date_origin,'-')[[1]][2]);
    YYYY=strtoi(year_begin);
  }
  if (calendar %in% c('gregorian')){
    TEMP=as.Date(time_day,origin=date_origin);
    DD  =as.numeric(format(TEMP,'%d'));
    MM  =as.numeric(format(TEMP,'%m'));
    YYYY=as.numeric(format(TEMP,'%Y'));
    rm(TEMP);
  }

  if (calendar %in% c('noleap','all_leap','360_day')){
    if (calendar %in% c('360_day')){
      GVL=360; #numbers of noleap years days
      GV=0:(GVL-1)# # generic vector
      DD=as.numeric(rep(c(1:30),12));
      MM=as.numeric(sort(rep(c(1:12),30)));
    }
    if (calendar %in% c('noleap')){
      GVL=365; #numbers of noleap years days
      GV=0:(GVL-1)# # generic vector
      TEMP=as.Date(GV,origin='1950-01-01');
      DD  =as.numeric(format(TEMP,'%d'));
      MM  =as.numeric(format(TEMP,'%m'));
      rm(TEMP);
    }
    if (calendar %in% c('all_leap')){
      GVL=366; #numbers of all leap years days
      GV=0:(GVL-1)# # generic vector
      TEMP=as.Date(GV,origin='1952-01-01');
      DD  =as.numeric(format(TEMP,'%d'));
      MM  =as.numeric(format(TEMP,'%m'));
      rm(TEMP);
    }
    min_val=min(time_day); nb_years_for_min_val=floor(min_val/GVL);
    max_val=max(time_day); nb_years_for_max_val=floor(max_val/GVL);
    nb_year_covered=nb_years_for_max_val-nb_years_for_min_val+1;
    real_first_year=strtoi(year_begin)+nb_years_for_min_val;
    real_last_year =strtoi(year_begin)+nb_years_for_max_val;
    time_day_shift=time_day-(nb_years_for_min_val*GVL);
    list_year=seq(real_first_year,real_last_year,1);
    list_year=matrix(data=list_year,nrow=1,ncol=length(list_year));
    YYYY=c(apply(list_year,2,rep,GVL));
    MM  =rep(MM,nb_year_covered);
    DD  =rep(DD,nb_year_covered);
    # ADD +1 because is an index of vector
    YYYY=YYYY[time_day_shift+1];
    MM  =  MM[time_day_shift+1];
    DD  =  DD[time_day_shift+1];
  }

  if (calendar %in% c('julian')){
    GVL=365.25;
    min_val=min(time_day); nb_years_for_min_val=floor(min_val/GVL);
    max_val=max(time_day); nb_years_for_max_val=floor(max_val/GVL);
    nb_year_covered=nb_years_for_max_val-nb_years_for_min_val+1;
    real_first_year=strtoi(year_begin)+nb_years_for_min_val;
    real_last_year=strtoi(year_begin)+nb_years_for_max_val;
    we_begin_at_year=real_first_year-real_first_year%%4;
    we_finish_at_year=real_last_year+(0.75-(real_last_year/4)%%1)*4;
    we_cover_4times_nb_year=(we_finish_at_year-we_begin_at_year+1)/4;
    if (we_begin_at_year<year_begin){
      list_shift_year=(we_begin_at_year:(strtoi(year_begin)-1));
      nby_366=sum((list_shift_year/4)%%1==0);
      nby_365=length(list_shift_year)-nby_366;
      time_day_shift=time_day+365*nby_365+366*nby_366;
      new_date_origin=paste0(we_begin_at_year,substr(date_origin,(nchar(date_origin)-5),nchar(date_origin)));
    } else if (we_begin_at_year>strtoi(year_begin)){
      list_shift_year=(strtoi(year_begin):we_finish_at_year);
      nby_366=sum((list_shift_year/4)%%1==0);
      nby_365=length(list_shift_year)-nby_366;
      time_day_shift=time_day-(365*(nby_365-3)+366*(nby_366-1));
      new_date_origin=paste0(we_begin_at_year,substr(date_origin,(nchar(date_origin)-5),nchar(date_origin)));
    } else {
      list_shift_year=strtoi(year_begin);
      new_date_origin=date_origin;
      time_day_shift=time_day;
    }
    TEMP=as.Date(0:1460,origin=new_date_origin);
    DD=as.numeric(format(TEMP,'%d'));
    MM=as.numeric(format(TEMP,'%m'));
    YYYY=as.numeric(format(TEMP,'%Y'));
    DD=rep(DD,we_cover_4times_nb_year);
    MM=rep(MM,we_cover_4times_nb_year);
    for (yy in 1:(we_cover_4times_nb_year-1)){YYYY=c(YYYY,(YYYY+4));};
    MM  =  MM[time_day_shift+1];
    DD  =  DD[time_day_shift+1];
    YYYY=YYYY[time_day_shift+1];
  }
  YMD=YYYY*10^4+MM*10^2+DD;

#source('~/Rstat/Functions/whos2.R');
#whos2()


  return(list(YMD=YMD,YYYY=YYYY,MM=MM,DD=DD));
}
###############################################################################
###############################################################################
#  --------- Human readable YYYY-MM-DD ----------------------------------------
#  Create human readable YYYY-MM-DD (YEAR-MONTH-DAY)
# Create a an human readable hh:mm:ss (ss can be integer or float with 3 digit only)
# -----------------------------------------------------------------------------
yymmdd_printer <- function(yymmdd){
# -----------------------------------------------------------------------------
# INPUT :
# yymmdd  = list as yymmdd_conversion return
# -----------------------------------------------------------------------------
# OUTPUT :
# character : for each time steps informations as 'YYYY-MM-DD'
# -----------------------------------------------------------------------------
  return(apply(cbind(yymmdd$YYYY,yymmdd$MM,yymmdd$DD),1,function(x){sprintf("%04d-%02d-%02d",x[1],x[2],x[3])}));
}
###############################################################################

