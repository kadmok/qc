library(dplyr)
library(data.table)
library(igraph)
library(purrr)
library(rlist)
library(tidyr)
library(foreach)
library(arrangements)
library(Rcpp)
library(tidyverse)
library(sparklyr)


#has to be dataframe

begin<-Sys.time()
readtime<-Sys.time()

#filename<-'dir/bms2_100.dat'
#filename<-'dir/bms2_50.dat'
#filename<-'dir/bms2_30.dat'
#filename<-'dir/bms2_400.dat'
#filename<-'dir/bms2_800.dat'
#filename<-'dir/ex1.txt'
#filename<-'dir/holypaper_2.txt'
filename<-'dir/mushroom.dat'
#filename<-'dir/breast-cancer-wisconsin_bi.dat'
#filename<-'dir/chess.dat'
#filename<-'dir/nursery_bi.dat'
#filename<-'dir/soybean-large_bi.dat'
#filename<-'dir/house-votes-84_bi.dat'
ncol <- max(count.fields(filename, sep = " "))
tr<-read.table(filename, header=F, sep=' ', fill=T, col.names=paste0('V', seq_len(ncol)))
#tr<-  read.table(filename, header=F,stringsAsFactors=FALSE, )


#gc()
#get last column
#get highest value in last column
#_______________________________________________________________-----------------
#ncol-1 bc last column was na
maxcol<-max(tr[, ncol-1], na.rm = TRUE)
maxrow<-nrow(tr)
#maxcol<-max(tr, na.rm = TRUE)

#_______________________________________________________________-----------------------------

cat( " " ,file="timefilemarch.txt",append=TRUE,sep="\n")
cat( "2403ver" ,file="timefilemarch.txt",append=TRUE,sep="\n")
cat(format(Sys.Date(), format="%B %d %Y") ,file="timefilemarch.txt",append=TRUE,sep="\n")
cat(filename ,file="timefilemarch.txt",append=TRUE,sep="\n")



ds<-matrix( rep( FALSE, len=maxcol*maxrow), nrow = maxrow)
covered<-matrix( rep( FALSE, len=maxcol*maxrow), nrow = maxrow)
for(y in 1:maxrow) {
  for(x in tr[y,]) {
      ds[y,x]=TRUE
  }}
print(Sys.time()-readtime)



 
 
QC1403<-function(datm, covm){ 
  qc<-Sys.time()
  
  numofc<-0
  #mandatory
  qc<-Sys.time()
  trues<-which(datm==TRUE, arr.ind = TRUE)

  #size
  qc<-Sys.time()
  trues<-cbind(trues, c(0))

  for (nr in 1:length(trues[,1])){

    trues[nr,3]<-sum(datm[,trues[nr,2]])* sum(datm[trues[nr,1],])
        } 
  trues<-trues[order(trues[, 3]), ]


print("sizetime")
print(Sys.time()-qc)
sizetime <- difftime(Sys.time(),qc,units="secs")
cat("sizetime" ,file="timefilemarch.txt",append=TRUE,sep="\n")
cat(sizetime ,file="timefilemarch.txt",append=TRUE,sep="\n")


  # non mandatory
  qc<-Sys.time()
  for (i in 1:nrow(trues)){
    #if not covered
    if(!covm[trues[i, 1], trues[i, 2]]){
      numofc<-numofc+1
      
      
      
      extents<-trues[trues[,2]==trues[i, 2],1]
      intents<-c()
      for(xn in 1:length(datm[1,])){
        if(sum(datm[datm[,trues[i, 2]] == TRUE,xn])==sum(datm[,trues[i, 2]]))
          intents<-c(intents, xn)        }
      
      # cat("\n")
      # print("INTS")
      # print(unique(intents))
      # print("EXTS")
      # print(extents)
      
      for(int in intents){
        for( e in extents){
          covm[e, int]<-TRUE
        }}
    }
    
  }

  print("concepts")
  print(Sys.time()-qc)
  print(numofc)
  cat("num of concepts" ,file="timefilemarch.txt",append=TRUE,sep="\n")
  cat(numofc ,file="timefilemarch.txt",append=TRUE,sep="\n")
  }


  
qc<-Sys.time()
QC1403(ds, covered)  
print(Sys.time()-qc)
print("total time")
print(Sys.time()-begin)

tot <- difftime(Sys.time(),begin,units="secs")
cat("total time" ,file="timefilemarch.txt",append=TRUE,sep="\n")
cat(tot,file="timefilemarch.txt",append=TRUE,sep="\n")

