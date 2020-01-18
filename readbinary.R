
#read binaty data from folder
em<- do.call("rbind", lapply(dir("BasesBinaires", full.names = TRUE), read.table,  skip = 2,  as.is = TRUE))
colnames(em) <- c(paste0( colnames(em), "c"))
rows<-row.names(em)
cols<-colnames(em)

id <- rownames(em)
em <- cbind(id=id, em)


eedges<- melt(as.data.table(em))
eedges <- dplyr::filter(eedges, value =="1")
datedges<-eedges
datedges<-distinct(eedges)
colnames(datedges)[1]<- "y"
colnames(datedges)[2]<- "x"
