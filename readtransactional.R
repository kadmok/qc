
#read transactional data from folder
tr<- do.call("rbind", lapply(dir("BasesBinaires", full.names = TRUE), read.table,    as.is = TRUE))
transactionaledges <- data.frame(y=NA, x=NA, value=NA)
 
for(y in 1:nrow(tr)){
  for(col in colnames(tr)){
    x<- paste("V",tr[y, col],"c", sep="")
    
    transactionaledges<- rbind(transactionaledges, c(y=y, x=x, value=1) )
  }
}
datedges<-transactionaledges