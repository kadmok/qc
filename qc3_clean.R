library(dplyr)
library(sparklyr)
library(data.table)
library(igraph)
library(purrr)
library(rlist)
library(tidyr)
library(foreach)
library(tidyverse)
library(arrangements)


totaltime<-Sys.time()

#extent and intent formula
getextentsingle<- function(x1, data){
  return( data %>% dplyr::filter(x==x1) %>% dplyr::distinct(y) )
}
#getextentsingle("e",datedges)


getintentsingle<- function(y1, data){
  return( data %>% dplyr::filter(y==y1) %>% dplyr::distinct(x) )
}


#bit faster
getintentsingle2<- function(y1, data) {
  intents <- data %>% dplyr::filter(y==y1) %>% dplyr::distinct(x)
  returnlist <- list()

  for (i in intents$x){
    returnlist <- append(returnlist, i)  }
  return (unlist(unique(returnlist))) }





#have to make this one faster
getintent<- function(x, data){
  extent<-getextentsingle(x, data)
  extsize<-nrow(extent)
  returnlist <- list()

  for (i in extent$y){
    intentsingle <- getintentsingle(i, data)
    for (n in intentsingle$x){
      returnlist<-c(returnlist, (lastloop_getintent(n,data,extent,extsize)))
    }     }
  return (unlist(unique(returnlist)))}




#have to make this one faster
getintent3<- function(x, data){
  extent<-getextentsingle(x, data)
  extsize<-nrow(extent)
  returnlist <- list()
  
  for (i in extent$y){
    intentsingle <- getintentsingle2(i, data)
    for (n in intentsingle){
      returnlist<-c(returnlist, (lastloop_getintent(n,data,extent,extsize)))
    }     }
  return (unlist(unique(returnlist)))}


lastloop_getintent<- function(n, data, ext, extsize){
  extentsingle <- getextentsingle(n, data)
  if (length( ext$y[na.omit(match(ext$y, extentsingle$y))] )==extsize)  {  
    return(paste("",n,sep=""))
  }}



gc()
t1<-Sys.time()
intentht<- new.env()
for (i in colnames(em)){
  assign(i, getintent3(i, datedges), intentht)
  
}
print( Sys.time()- t1)
hashtime <- difftime(Sys.time(),t1,units="secs")



 cat("hashtable time" ,file="timefile.txt",append=TRUE,sep="\n")
 cat(hashtime ,file="timefile.txt",append=TRUE,sep="\n")



getintersect<-function(i, extenth){
  lugeja<-0
  lugeja <- lugeja + length(intersect(extenth, extentht[[i]]))
  return(lugeja)
}

#new size formula - everything from hashtables
#calculate size
getsizehash<- function(x, y, data){
  xn <- paste("",x,sep="")
  p<-paste("i",y,sep="")
  intent <-  intentsingleht[[p]]
  nimetaja <-length(extentht[[xn]])*length(intent)
  lugeja <- 0
  
  #find intersect for each extent of intent
  for (i in intent){
    lugeja<-lugeja+getintersect(i, extentht[[xn]])
  }
  return(lugeja/nimetaja)
}

is.contained <- function(x, y) {
  z <- x[x %in%setdiff(x, y)]
  length(z) == length(x) - length(y)
}




# t1<-Sys.time()
# hiddenconcept_smart("V2c" , "i10")
# # t1<-Sys.time()
# getsizehash("V2c" , 10, 1)
# print( Sys.time()- t1)
# 
# 
# t1<-Sys.time()
# size_concept("V2c" , 10)
# print( Sys.time()- t1)


#size and best concept from the same calculation
size_concept<-function(x, y){
  yn<-paste("i",y,sep="")
  xn <- paste("",x,sep="")
  sizeNumerator<-length(intentht[[xn]])
  
  unionlist <- list()
  for (item in intentht[[xn]]){
    unionlist <- append(unionlist, extentht[[item]]) }
  unionlist <- unlist(unique(unionlist))
  
  maxbond<-length(extentht[[xn]])/length(unionlist)
  bond<-maxbond
  bestFC<-list(intentht[[xn]], extentht[[xn]])
  fintent<-list()
  fextent<-list()

  for (i in intentsingleht[[yn]]){
    sparkingC<-FALSE
    #print(i)
    if(is.contained( extentht[[xn]], extentht[[i]] )){
      fintent<-intentht[[i]]
      fextent<-extentht[[i]]
    }
    else{
      if(is.contained( extentht[[i]],extentht[[xn]])){
        fintent<-intentht[[xn]]
        fextent<-extentht[[xn]]
        sparkingC<-TRUE
      }
      else{
        fintent<-intersect(intentht[[xn]],intentht[[i]])
        fextent<- intersect(extentht[[xn]], extentht[[i]])
      }
    }
    sizeNumerator<-sizeNumerator+length(fextent)
    
    if(sparkingC==FALSE){
      unionlist <- list()
      for (item in fintent){
        unionlist <- append(unionlist, extentht[[item]]) }
      unionlist <- unlist(unique(unionlist))
      unionlist_len<-length(unionlist)
      
      if(unionlist_len>0){
        bond<-length(fextent)/unionlist_len}
    }
    
    if(bond>maxbond & y %in% fextent & xn %in% fintent){
      maxbond<-bond
      bestFC<-list(fintent, fextent)
    }  }
  size<-sizeNumerator/(length(extentht[[xn]])*length(intentsingleht[[yn]]))
  return(c(size, bestFC))
}


#hashtable for extent
extentht<- new.env()
for (i in colnames(em)){
  p<-i
  value<-getextentsingle(i, datedges)$y
  assign(p, value, extentht)
}

#memory release
gc()



t1<-Sys.time()
#hashtable for intent(y)
intentsingleht<- new.env()
for (i in rownames(em)){
  p<-paste("i",i,sep="")
  value<-getintentsingle2(i, datedges)
  assign(p, value, intentsingleht)
}
print( Sys.time()- t1)


gc()

#hashables for support
supportx<- new.env()
for (i in colnames(em)){
  p<-i
  value<-length(extentht[[i]])
  assign(p, value, supportx)
}



supporty<- new.env()
for (i in rownames(em)){
  p<-paste("i",i,sep="")
  value<-length(intentsingleht[[p]])
  assign(p, value, supporty)
}


gc()



hiddenconcept_smart<- function(xn, yn){
  returnlist <- list()
  bondvalue <- 0
  for( i in intentht[[xn]]){
    returnintent<- intentht[[i]]
    
    if(xn %in% returnintent){
      returnextent <- extentht[[i]]
      newbond <- bondcalc(returnintent, extentht[[i]])
      
      if(newbond>bondvalue){
        bondvalue<-newbond
        returnlist <- append(list(returnintent),list(returnextent)) }
    }    }
  return(returnlist)}




bondcalc <- function(intent, extent){
  unionlist <- list()
  intersectlist_len <- length(extent)
  for (i in intent){
    unionlist <- append(unionlist, extentht[[i]])  }
  
  unionlist <- unlist(unique(unionlist))
  unionlist_len<-length(unionlist)
  return(intersectlist_len/unionlist_len) }


gc()
#need to make this one faster
#data structure
dattime<-Sys.time()

ext_int_table <- data.frame("item"= unique(datedges$x) )
for (i in 1:nrow(ext_int_table)){
  xn<-paste("",ext_int_table$item[i],sep="")
  extent<-(extentht[[xn]])
  
  ext_supp <- list()
  for (e in 1:length(extent)[1]){
    p<- paste("i",(extentht[[xn]])[e],sep="")
    ext_supp[e]<-supporty[[p]][1]
    gc()
  }
  ext_supp_temp <- ext_supp
  
  ext_int_table$len_extent[i]<-length(extentht[[xn]])
  ext_int_table$len_intent[i]<-length(intentht[[xn]])
  ext_int_table$intent[i]<-list(intentht[[xn]])
  ext_supp <- as.data.frame(lapply(ext_supp, unlist))
  i1 <- order(ext_supp)
  ext_lst <- unlist(extentht[[xn]], use.names=FALSE)
  ext_lst <-(ext_lst)[i1]
  

  ext_supp_temp <- unlist(ext_supp_temp)[i1]
  ext_int_table$extent[i]<-list(ext_lst)
  ext_int_table$ext_supp[i]<-list(ext_supp_temp)
}
ext_int_table<-ext_int_table[!duplicated(ext_int_table$extent), ]


datstructuretime<- (difftime( Sys.time(), dattime, units="secs"))
print(datstructuretime)
 cat("big data structure table creation:" ,file="timefile.txt",append=TRUE,sep="\n")
 cat(datstructuretime ,file="timefile.txt",append=TRUE,sep="\n")



gc()






singlecouples_conceptsqcover3_hashtable_noprint<- function(t1){
  if(nrow(t1)>0 ){
    x<-bestconcept[[t1$couple[1]]][[1]]
    y<-bestconcept[[t1$couple[1]]][[2]]
    for(ix in x){
      for(iy in y){
        p<-paste(ix,iy,sep="")
        t1<- t1[ t1$couple!= p ]      }}
    return(singlecouples_conceptsqcover3_hashtable_noprint(t1)+1)  }
  else{return(0)}}



qcover3_noprint<- function(tabl, itemcount){
  start_time <- Sys.time()
  tbl1 <- data.frame()
  num=0
  #first part - mandatory concepts
  for(i in 1:nrow(tabl)){
    if (tabl$len_extent[i]==1 | tabl$len_intent[i] %in% tabl$ext_supp[i][[1]]){
      num=num+1
      for(ix in tabl$intent[i][[1]]){
        for(iy in tabl$extent[i][[1]]){
          p<-paste(ix,iy,sep="")
          rm(list=p ,envir = coveredenv)}}
      itemcount<- itemcount+1 }  }
  
  firstpart_end_time <- Sys.time()
  sizetime<-0
  tbltime<-0
  
  print("mandatory concepts:")
  print(num)
  
  #second part of algorithm
  if(nrow(tabl)>itemcount){
    tablelist <- list()
    sizelist <- list()
    fcintent <- list()
    fcextent <- list()
    
    for (couple in ls(coveredenv)){
      s<-Sys.time()
      size_concept<-size_concept(coveredenv[[couple]][[1]],coveredenv[[couple]][[2]])
      b<-Sys.time()

      couplename<-paste(coveredenv[[couple]][[1]],coveredenv[[couple]][[2]],sep="")
      assign(couplename, list( size_concept[[2]],  size_concept[[3]]), bestconcept)
      
      tbl1<- rbind(tbl1, data.table( size=size_concept[[1]], couple=couplename ) )
     t<-Sys.time()
      tbltime<- tbltime+ difftime(t,b, units="secs")
      sizetime<- sizetime+ difftime(b,s, units="secs")
    }

    print(nrow(tbl1))
    cat("----qc3 size and best FC together-----" ,file="timefile.txt",append=TRUE,sep="\n")
    
    # cat(nrow(tbl1) ,file="timefile.txt",append=TRUE,sep="\n")
    tbl1 <- tbl1[order(-tbl1$size),]
    secondpart_end_time <- Sys.time()
    
    print("timeoftableadd")
    print(tbltime)
    print("timeofsizecalc")
    print(sizetime)
    print("sec part")
    print(difftime( secondpart_end_time, firstpart_end_time, units="secs"))

    num=num+ singlecouples_conceptsqcover3_hashtable_noprint(tbl1)
  }
  thirdpart_end_time <- Sys.time()
  
  total <-  difftime(thirdpart_end_time,start_time, units="secs")
  firstpart <-  difftime(firstpart_end_time, start_time, units="secs")
  secondpart <-  difftime( secondpart_end_time, firstpart_end_time, units="secs")
  thirdpart <- difftime(thirdpart_end_time,secondpart_end_time,units="secs")
  print("third part")
  print(thirdpart)
  
  times <- list(total, firstpart, secondpart, thirdpart )
  
  cat("total time in seconds" ,file="timefile.txt",append=TRUE,sep="\n")
  cat(total ,file="timefile.txt",append=TRUE,sep="\n")
  cat("first part in seconds" ,file="timefile.txt",append=TRUE,sep="\n")
  cat(firstpart ,file="timefile.txt",append=TRUE,sep="\n")
  cat("second part in seconds" ,file="timefile.txt",append=TRUE,sep="\n")
  cat(secondpart,file="timefile.txt",append=TRUE,sep="\n")
  cat("including size" ,file="timefile.txt",append=TRUE,sep="\n")
  cat(sizetime,file="timefile.txt",append=TRUE,sep="\n")
  cat("third part in seconds" ,file="timefile.txt",append=TRUE,sep="\n")
  cat(thirdpart  ,file="timefile.txt",append=TRUE,sep="\n")
  cat("num of concepts" ,file="timefile.txt",append=TRUE,sep="\n")
  cat(num  ,file="timefile.txt",append=TRUE,sep="\n")
  
  return(c(times, num))  }



coveredenv<- new.env()
ls.str(coveredenv)
ls(coveredenv)
for (i in 1:nrow(datedges)){
  p<-paste(datedges$x[i],datedges$y[i],sep="")
  assign(p, list( as.character(datedges$x[i]),  datedges$y[i]), coveredenv)
}
bestconcept<- new.env()
print(qcover3_noprint(ext_int_table,  0))



print("total time from start to end in seconds")
print( Sys.time()- totaltime)
tot <- difftime(Sys.time(),totaltime,units="secs")
cat("total time from start to end in seconds" ,file="timefile.txt",append=TRUE,sep="\n")
cat(tot  ,file="timefile.txt",append=TRUE,sep="\n")



