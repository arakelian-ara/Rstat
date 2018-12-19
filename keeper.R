###############################################################################
# keeper function and wrapper
# v1.0 by Ara Arakelian (LSCE) Dec 2018
###############################################################################
#  --------- keeper -----------------------------------------------------------
#  Function to keep only specified variables.
#  Can handle function, keep some of them or not.
#  To be used into the "main" script, may also be used into function
# -----------------------------------------------------------------------------
keeper<-function(...,chara=character(),fct=TRUE,my_envir=parent.env(environment())){
# -----------------------------------------------------------------------------
# Examples :
# Easy to use : keeper(a,b,c,d) 
#               keeper('a','b','c','d')
#            or keeper(a,b,'c','d')
# Use with function : keeper(a,b,fct_a,fct_b,fct=FALSE)
#                  or keeper(a,'b',fct_a,'fct_b',fct=FALSE)
# Using chara : keeper(a,'b',chara=c('c','d'));
# chara is designed to be used as the results of the ls function, such as :
# > list_var=ls(pattern='a')
# > keeper(chara=list_var)
# INPUT -----------------------------------------------------------------------
# ...   = string : as a,b,c,d
# chara = string : as c('a','ab','ac')
# fct   = bool   : if TRUE, keep all function
#                  if FALSE, keep function only if named
# my_envir = environment : by default parent environnement
# OUTPUT --------------------------------------------------------------
# No output
# ---------------------------------------------------------------------
# Notes :
# All hidden elements (which begin with a dot) must be explicitly named 
# to keep them, so be careful with hidden functions.
# There is an invisible call of garbage collection at the end.
# ---------------------------------------------------------------------
  ARG=c(as.character(substitute(list(...)))[-1],as.character(chara));
  list_fct=as.character(lsf.str(all.names=TRUE,envir=my_envir));# liste fonction only
  list_var=ls(all.names=TRUE,envir=my_envir);     # liste variable + fonction
  list_var=setdiff(list_var,list_fct);         # liste variable only
#\none variable or function names    
  if( length(ARG)==0 | (length(ARG)==1 & ARG[1]=='') ){
#\\\remove all variables & keep all functions  
    if(fct==TRUE)  rm(list=list_var,envir=my_envir);
#\\\remove all variables & remove all functions    
    if(fct==FALSE) rm(list=c(list_var,list_fct),envir=my_envir);
#\some variables and/or functions will be keep
  } else {
#\\\remove some variables & keep all functions  
    if(fct==TRUE)  rm(list=setdiff(list_var,ARG),envir=my_envir);
#\\\remove some variables & some functions
    if(fct==FALSE) rm(list=setdiff(c(list_var,list_fct),ARG),envir=my_envir);
  }
  invisible(gc());
}
###############################################################################
###############################################################################
#  --------- keeper wrapper with load -----------------------------------------
#  Kind of wrapper for keeper function.
#  Allow to load only wanted variables from file
#  /!\ all variables are loaded, but only those requested are deposited in the environment 
# -----------------------------------------------------------------------------
load_keeper<-function(file,...,chara=character(),my_envir=parent.env(environment())){
# -----------------------------------------------------------------------------
# Examples :
# Easy to use : keeper(file,a,b,c,d) 
#               keeper(file,'a','b','c','d')
#            or keeper(file,a,b,'c','d')
# Using chara : keeper(file,a,'b',chara=c('c','d'));
# chara is designed to be used as the results of the ls function, such as ls(pattern='a')
# INPUT -----------------------------------------------------------------------
# file  = string : path to R data file
# ...   = string : as a,b,c,d or 'a','b','c','d' or a,b,'c','d'
# chara = string : as c('a','ab','ac')
# envir = environment : by default parent environment
# OUTPUT --------------------------------------------------------------
# No output
# ---------------------------------------------------------------------
# Notes :
# Functions are untouched, we assume that there are only variables in the file.
# There is an invisible call of garbage collection at the end.
# --------------------------------------------------------------------
  env_tmp=new.env();
  var_from_file=load(file,envir=env_tmp);
  ARG=c(as.character(substitute(list(...)))[-1],as.character(chara));
  # Test if ask variables are in the file
  ttt=(!is.element(ARG,var_from_file));
  if(any(ttt)){
    warning('Some variables are not in the file, see above at load_keeper call');
    cat(sprintf('Some variables are not in the file:\n'));
    cat(sprintf('%s ',ARG[which(ttt)]))
    cat(sprintf('\n'));
  }
  keeper(chara=ARG,my_envir=env_tmp);
  for(i in ls(envir=env_tmp,all.names=TRUE)){assign(i,get(i,env_tmp),my_envir)}
}
###############################################################################
###############################################################################




