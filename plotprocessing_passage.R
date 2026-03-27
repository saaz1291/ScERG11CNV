plotprocessing_passage<-function(timecourse) {
  
  melted <- melt(timecourse,id.vars="Passage")
  melted$variable=as.character(melted$variable)
  Genotype=strsplit(melted$variable,split="_")
  Genotype=matrix(unlist(Genotype),ncol=2,byrow=TRUE)
  Genotype=Genotype[,1]
  melted=cbind(melted,Genotype)
  return(melted)
}

