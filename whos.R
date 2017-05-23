#######################################################################
# whos() function
# look like matlab
# v1.0 by Ara Arakelian (LSCE) February 2016
#######################################################################
# whos()
#######################################################################
  whos <- function(...){
# INPUT -------------------------------------------------------
# can be empty (alphabetic without function)
# combinaison of character s / r / f
# s = order by size
# r = reverse order
# f = add function to the printed list
# -------------------------------------------------------------
a=sapply(ls(.GlobalEnv), function (object.name) object.size(get(object.name)));
b=sapply(ls(.GlobalEnv), function (object.name) class(get(object.name)));
# -------------------------------------------------------------
arg=as.list(match.call());
if      (length(arg)==1){ kind='';}
else if (length(arg)==2){ kind=arg[[2]][1];}
else {   stop('nothing OR one (and only one) string with combinaison of f r s characters only'); }
rm(arg);
# -------------------------------------------------------------
F=0;S=0;R=0;
if (nchar(kind)!=0){
  if (grepl('f',kind,ignore.case=TRUE)==1){F=1};
  if (grepl('s',kind,ignore.case=TRUE)==1){S=1};
  if (grepl('r',kind,ignore.case=TRUE)==1){R=1};
}
# -------------------------------------------------------------
     if (S==0){ order=1:length(a); } # alphabetic
else if (S==1){ order=sort(as.integer(a),index.return=TRUE)$ix; }# by size
if (R==1){ order=rev(order);} # reverse
# -------------------------------------------------------------
cat('Memory used in bytes (1octet=1byte=8bits)\n')
if (F==1){
 cat('--- Variables --------------------------------\n',sep='');
}
for(i in order){
  if(as.character(b[i])!="function"){
    .whosNP(a[i]); .whosTY(a[i],b[i]); .whosLD(a[i]); cat('\n',sep='');
  }
}
if (F==1){
cat('--- Functions --------------------------------\n',sep='');
for(i in order){
  if(as.character(b[i])=="function"){
    .whosNP(a[i]); .whosTY(a[i],b[i]); cat('\n',sep='');
  }
}
}
rm(a,b);
}
#######################################################################
# Whos : print name + weight
#######################################################################
  .whosNP <- function(x){
cat(sprintf('%15s',names(x)),sep='');
T=1024;
if (x<=T){               cat(sprintf('%6.0f o',x)    ,sep='');}
else if (x>T   & x<=T^2){cat(sprintf('%6.0f k',x/T)  ,sep='');}
else if (x>T^2 & x<=T^3){cat(sprintf('%6.1f M',x/T^2),sep='');}
else if (x>T^3){         cat(sprintf('%6.2f G',x/T^3),sep='');}
}
#######################################################################
# Whos : print length or dimension 
#######################################################################
  .whosLD <- function(a){
nvb=parse(text=names(a));
if (is.character(eval(nvb)))  {cat(sprintf('%8d',nchar(eval(nvb)) ,sep='')); }
else if (is.vector(eval(nvb))){cat(sprintf('%8d',length(eval(nvb)),sep='')); }
else if (is.matrix(eval(nvb))){cat(sprintf('%8d x %d',dim(eval(nvb))[1],dim(eval(nvb))[2],sep='')); }
else if (is.array (eval(nvb))){cat(sprintf('%8d',dim(eval(nvb))[1],sep=''));
                               for (i in 2:length(dim(eval(nvb)))){
                                cat(sprintf(' x %d',dim(eval(nvb))[i],sep=''));
                               }
}
else { cat('   casetoadd');}
}
#######################################################################
# Whos : print type ie "kind"
#######################################################################
  .whosTY <- function(a,b){
if (as.character(b)=='numeric'){ 
  nvb=parse(text=names(a));
  if      (is.vector(eval(nvb))){cat(sprintf('%11s','vector',sep=''));}
  else if (is.array( eval(nvb))){cat(sprintf('%11s','array' ,sep=''));}
} else {                         cat(sprintf('%11s',as.character(b)),sep='');
}
}



