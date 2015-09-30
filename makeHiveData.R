makeHiveData<-function(data,colTest){
  n=dim(data)[1];
  ids=1:(3*n);
  lab=sapply(ids,as.character);
  axis=as.integer(c(rep(1,n),rep(2,n),rep(3,n)));
  radius=c(data[,1],data[,2],data[,3])
  size=rep(0.1,3*n);
  color=rep("#000000",3*n);
  nodes=data.frame(ids,lab,axis,radius,size,color,stringsAsFactors=FALSE);
  id1<-1:(3*n);
  id2<-as.integer((id1+n-1)%%(3*n)+1)
  weight<-rep(1,3*n)
  color=sapply(id1,function(x) ifelse(colTest(data[(x-1)%%n+1,1]),"#0000ff","#ff0000"))
  edges=data.frame(id1,id2,weight,color,stringsAsFactors=FALSE)
  desc<-sprintf("3 axes -- %d nodes -- %d edges",3*n,3*n)
  axis.cols=rep("#000000",3)
  template=ranHiveData(nx=3)
  template$nodes<-nodes
  template$edges<-edges
  template$desc<-desc
  template$axis.cols<-axis.cols
  return (template)
}