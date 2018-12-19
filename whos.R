###############################################################################
# whos() function
# v1.3 by Ara Arakelian (LSCE) Dec 2018
###############################################################################
#  --------- whos -------------------------------------------------------------
#  Function to print informations about variables, "a la matlab"
#  The First purpose is to display the weight of the variables to be able to remove 
#  heavy or useless variables if necessary.
# -----------------------------------------------------------------------------
whos <- function(Type='',envir=parent.env(environment())){
# -----------------------------------------------------------------------------
# Examples :
# Easy to use : whos()
# Use with function : whos('s')
#                     whos('sr')
#                     whos('f')
# INPUT -----------------------------------------------------------------------
# Type = string, can be
#               empty (alphabetic without function)
#               combinaison of character s / r / f
#               s = order by size
#               r = reverse order
#               f = add function to the printed list
# envir = environment, parent by default
# -----------------------------------------------------------------------------
  Oname=ls(env=envir);
  Osize=unname(sapply(ls(env=envir), function (object.name) object.size(get(object.name,env=envir))));
  Oclas=unname(sapply(ls(env=envir), function (object.name) class(get(object.name,env=envir))));
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
  if (environmentName(envir)=="R_GlobalEnv"){
    cat('[R_GlobalEnv]',sep='')
  }
  else{
    if (environmentName(envir)!=""){
      cat('[',environmentName(envir),']')
    }
    else {
      cat('[no name (local)]')
    }
  }
  cat('\n');
  cat('--- Variables -----------------------------------------------------------\n',sep='');
  for(i in order){
    if(as.character(Oclas[i])!="function"){
      .whosNP(Oname[i],Osize[i]); .whosTY(Oclas[i]); .whosLD(Oname[i],Oclas[i],envir); cat('\n',sep='');
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
###############################################################################
###############################################################################
# Whos : print name + weight
# -----------------------------------------------------------------------------
.whosNP <- function(name,size){
  cat(sprintf('%20s',name),sep='');
  T=1024;
  if (size<=T)                  {cat(sprintf('%6.0f o',size)    ,sep='');}
  else if (size>T   & size<=T^2){cat(sprintf('%6.0f k',size/T)  ,sep='');}
  else if (size>T^2 & size<=T^3){cat(sprintf('%6.1f M',size/T^2),sep='');}
  else if (size>T^3)            {cat(sprintf('%6.2f G',size/T^3),sep='');}
}
###############################################################################
###############################################################################
# Whos : print type ie "kind"
# -----------------------------------------------------------------------------
.whosTY <- function(clas){
  cat(sprintf('%12s',clas,sep=''));
}
###############################################################################
###############################################################################
# Whos : print length or dimension
# -----------------------------------------------------------------------------
.whosLD <- function(name,clas,envir){
  if (clas=='character'){
    if(length(get(name,env=envir))==1){
      cat(sprintf('%8d',nchar(get(name,env=envir)),sep=''));
    } else {
      cat(sprintf('%8d strings',length(get(name,env=envir)),sep=''));
    }
  }
  else if (clas=='list'){cat(sprintf('%8d',length(get(name,env=envir)),sep='')); }
  else if (clas=='numeric'){cat(sprintf('%8d',length(get(name,env=envir)),sep='')); }
  else if (clas=='integer'){cat(sprintf('%8d',length(get(name,env=envir)),sep=''));}
  else if (clas=='matrix'){cat(sprintf('%8d x %d',dim(get(name,env=envir))[1],dim(get(name,env=envir))[2],sep='')); }
  else if (clas=='array'){cat(sprintf('%8d',dim(get(name,env=envir))[1],sep=''));
                           for (i in 2:length(dim(get(name,env=envir)))){
                             cat(sprintf(' x %d',dim(get(name,env=envir))[i],sep=''));
                           }
                         }
  else if (clas=='logical'){
    if(length(get(name,env=envir))==1){
      cat(sprintf('%8d : ',length(get(name,env=envir)),sep=''),get(name,env=envir),sep='');
    } else {
      cat(sprintf('%8d values',length(get(name,env=envir)),sep=''));
    }
  }
  else { cat('       case_to_add');}

  # If numeric or integer with only 1 value, the value is printed
  if((all(clas=='numeric') | all(clas=='integer'))){
    if(length(get(name,env=envir))==1){
      cat(sprintf(' : %g',get(name,env=envir)));
    }
  }
  # If there is 1 character string and is shorter than 20, the string is printed
  if(all(clas=='character') & length(get(name,env=envir))==1){
    if(nchar(get(name,env=envir))<20){
      cat(sprintf(' : \'%s\'',get(name,env=envir)));
    }
  }
}
###############################################################################
###############################################################################
