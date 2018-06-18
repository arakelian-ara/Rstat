#######################################################################
# whos() function
# look like matlab
# v1.1 by Ara Arakelian (LSCE) June 2018
#######################################################################
# whos()
#######################################################################
whos <- function(Type='',niv_env=-1){
# INPUT -------------------------------------------------------
# Type = string, can be
#               empty (alphabetic without function)
#               combinaison of character s / r / f
#               s = order by size
#               r = reverse order
#               f = add function to the printed list
# niv_env = integer
# -------------------------------------------------------------
  MyEnv=sys.frame(which=niv_env);
  Oname=ls(env=MyEnv);
  Osize=unname(sapply(ls(env=MyEnv), function (object.name) object.size(get(object.name,env=MyEnv))));
  Oclas=unname(sapply(ls(env=MyEnv), function (object.name) class(get(object.name,env=MyEnv))));
  # -------------------------------------------------------------
  FF=0;SS=0;RR=0;
  if (nchar(Type)!=0){
    if (grepl('f',Type,ignore.case=TRUE)==1){FF=1};
    if (grepl('s',Type,ignore.case=TRUE)==1){SS=1};
    if (grepl('r',Type,ignore.case=TRUE)==1){RR=1};
  }
  # -------------------------------------------------------------
  if (SS==0){order=1:length(Oname);} # alphabetic
       else {order=sort(Osize,index.return=TRUE)$ix;}# by size
  if (RR==1){order=rev(order);} # reverse
  # -------------------------------------------------------------
  cat('-------------------------------------------------------------------------\n',sep='');
  cat('Memory used in bytes (1octet=1byte=8bits) | Environment ',sep='')
  if (environmentName(MyEnv)=="R_GlobalEnv"){
    cat('[R_GlobalEnv]',sep='')
  }
  else{
    if (environmentName(MyEnv)!=""){
      cat('[',environmentName(MyEnv),']')
    }
    else {
      cat('[no name (local)]')
    }
  }
  cat('\n');

  cat('--- Variables -----------------------------------------------------------\n',sep='');
  for(i in order){
    if(as.character(Oclas[i])!="function"){
      .whosNP(Oname[i],Osize[i]); .whosTY(Oclas[i]); .whosLD(Oname[i],Oclas[i],MyEnv); cat('\n',sep='');
    }
  }
  if (FF==1){
    cat('--- Functions -----------------------------------------------------------\n',sep='');
    for(i in order){
      if(as.character(Oclas[i])=="function"){
        .whosNP(Oname[i],Osize[i]); .whosTY(Oclas[i]); cat('\n',sep='');
      }
    }
  }
  cat('-------------------------------------------------------------------------\n',sep='');
  rm(Oname,Osize,Oclas);
  invisible(gc());
}
#######################################################################
# Whos : print name + weight
#######################################################################
.whosNP <- function(name,size){
  cat(sprintf('%20s',name),sep='');
  T=1024;
  if (size<=T)                  {cat(sprintf('%6.0f o',size)    ,sep='');}
  else if (size>T   & size<=T^2){cat(sprintf('%6.0f k',size/T)  ,sep='');}
  else if (size>T^2 & size<=T^3){cat(sprintf('%6.1f M',size/T^2),sep='');}
  else if (size>T^3)            {cat(sprintf('%6.2f G',size/T^3),sep='');}
}
#######################################################################
# Whos : print type ie "kind"
#######################################################################
.whosTY <- function(clas){
  cat(sprintf('%11s',clas,sep=''));
}
#######################################################################
# Whos : print length or dimension
#######################################################################
.whosLD <- function(name,clas,MyEnv){
     if (clas=='character'){cat(sprintf('%8d',nchar(get(name,env=MyEnv)),sep=''));}
else if (clas=='list')     {cat(sprintf('%8d',length(get(name,env=MyEnv)),sep='')); }
else if (clas=='integer')  {cat(sprintf('%8d',length(get(name,env=MyEnv)),sep=''));}
else if (clas=='matrix')   {cat(sprintf('%8d x %d',dim(get(name,env=MyEnv))[1],dim(get(name,env=MyEnv))[2],sep='')); }
else if (clas=='array')    {cat(sprintf('%8d',dim(get(name,env=MyEnv))[1],sep=''));
                              for (i in 2:length(dim(get(name,env=MyEnv)))){
                                cat(sprintf(' x %d',dim(get(name,env=MyEnv))[i],sep=''));
                              }
                            }
  else                      { cat('       case_to_add');}
}

