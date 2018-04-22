###########################################################################################
# Simple script to unit test of time_handler.R script
# by Ara Arakelian - v1 - April 2018
# To be used on mesocentre.ipsl.fr ciclad/climserv
# Check if time_handler.R work with all kind of file
###########################################################################################
library('ncdf4')
source('~/Rstat/0HeatWaves/Functions/handle_files.R')
source('~/Rstat/0HeatWaves/Functions/whos.R')
source('~/Rstat/traitement_time_axis_function.R');
keep('');
###########################################################################################
# Fonction test
proto_test <- function(file,varn='time',sub_daily_info='auto'){
  system('clear');
  source('~/Rstat/traitement_time_axis_function.R');
  print(file)
  system(paste0('cdo sinfo ',file,'|tail -n36'));
  print('-------------------------------------------------------------------------------');
  print('-------------------------------------------------------------------------------');
  test=nc_time_handler(file,varn,sub_daily_info);
  time_handler_printer(test)
}
###########################################################################################

stop('');

FF='/data/aarakeli/ECOACT/P2/tas_MNA-22_NOAA-GFDL-GFDL-ESM2M_historical_r1i1p1_SMHI-RCA4_v1_day_19660101-19701231.nc'
proto_test(FF,'time')
my_time=nc_time_handler(FF,'time',TRUE,'gregorian',"days since 1966-01-01 00:00:00",seq(0.5,1825.5,1));
time_handler_printer(my_time)


file='/data/aarakeli/STORMS/Data/MED_ERAI_REGM/RAW/vas_MED-44i_ECMWF-ERAINT_evaluation_r1i1p1_ENEA-REGCM_v31_6hr_199401-199412.nc';
proto_test(FF,'time');

FF='/data/aarakeli/ECOACT/P2/tas_MNA-22_NOAA-GFDL-GFDL-ESM2M_historical_r1i1p1_SMHI-RCA4_v1_day_19660101-19701231.nc'
proto_test(FF,'time');

FF='/bdd/ncep/4xdaily/sigma/2000/vair.spec.2000.nc'
proto_test(FF);



###########################################################################################
# EVERYTHING GO FINE
###########################################################################################
if(FALSE){#################################################################################
###########################################################################################
# FICHIER OK
FF=list();
a=0;
a=a+1;FF[[a]]='/data/aarakeli/STORMS/Data/MED_ERAI_REGM/RAW/vas_MED-44i_ECMWF-ERAINT_evaluation_r1i1p1_ENEA-REGCM_v31_6hr_199401-199412.nc'
a=a+1;FF[[a]]='/data/aarakeli/STORMS/Data/EUR_ERAI_hadGEM3/RAW/va850_EUR-44_ECMWF-ERAINT_evaluation_r1i1p1_MOHC-HadGEM3-RA_v1_6hr_2002010100-2002123118.nc';
a=a+1;FF[[a]]='/data/aarakeli/STORMS/Data/MED_ERAI_CCLM/RAW/sfcWind_MED-44_ECMWF-ERAINT_evaluation_r1i1p1_GUF-CCLM4-8-18_v1_3hr_2005010100-2005020100.nc'
a=a+1;FF[[a]]='/data/aarakeli/STORMS/Data/MED_ERAI_CCLM/RAW/psl_MED-44_ECMWF-ERAINT_evaluation_r1i1p1_GUF-CCLM4-8-18_v1_3hr_2006080100-2006090100.nc';
a=a+1;FF[[a]]='/data/aarakeli/HEATWAVES/Data/ECAD25v16_his/rr_19500101-19991231.nc'
a=a+1;FF[[a]]='/data/aarakeli/SEENDM/Data/ECAD25_ful/RAW/pp_0.25deg_reg_v11.0.nc'
a=a+1;FF[[a]]='/data/aarakeli/ECOACT/P1/tas_85_mon_204101-205012.nc';
a=a+1;FF[[a]]='/data/aarakeli/FFSA/DATA/CORDEX11_ERAI/pr_1989_2008_r.nc'
a=a+1;FF[[a]]='/data/aarakeli/OASIS/ERAI025N/v10_19790101_20160630.nc'
a=a+1;FF[[a]]='/data/aarakeli/OASIS/ERAI075N/u10_19790101_20160630.nc'
a=a+1;FF[[a]]='/data/aarakeli/SEENDM/Data/NARR/RAW/air.2m.1986.nc'
a=a+1;FF[[a]]='/data/aarakeli/SEENDM/Data/NARR/RAW/air.2m.1988.nc'
a=a+1;FF[[a]]='/bdd/ncep/4xdaily/surf/1948/slp.1948.nc'

for (ff in 1:a){readline(prompt="Press [enter] to continue");proto_test(FF[[ff]]);readline(prompt="Press [enter] to continue");}

###########################################################################################
FF='/bdd/E-OBS/Grid_0.25deg_reg/tx_0.25deg_reg_v15.0.nc4';  proto_test(FF);
FF='/bdd/ncep/4xdaily/press/2010/rhum.2010.nc' ;            proto_test(FF);
FF='/bdd/ncep/1xdaily/press/2010/rhum.2010.nc' ;            proto_test(FF);
FF='/bdd/ncep/4xdaily/flux/1995/tmax.2m.gauss.1995.nc' ;    proto_test(FF);
FF='/bdd/ncep/4xdaily/press/2010/rhum.2010.nc' ;            proto_test(FF,'time',TRUE);
###########################################################################################
# 'time' is 'time_counter'
FF='/data/aarakeli/THESE_SAVE/download_CM5MR/v5.historicalMR1_19500101_19591231_1D_ua.nc'; proto_test(FF,'time_counter');
FF='/data/aarakeli/THESE_SAVE/download_CM5MR/v5.historicalMR1_19500101_19591231_1D_ua.nc'; proto_test(FF,'time_counter');
###########################################################################################
}##########################################################################################
###########################################################################################
#
