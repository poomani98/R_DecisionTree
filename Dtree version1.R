entropy = function(y,x=NULL){
  if(is.null(x)){
    y = y/sum(y)
    y[y==0]=1
    return(-sum(y*log2(y)))
  }
  else{
    t=table(x,y)
    return((rowSums(t)/length(y))%*%apply(t,1,FUN=entropy))
  }
}
infogain = function(data){
  col = ncol(data)
  target = entropy(table(data[,col]))
  return(target - apply(data[,-col],2,FUN=function(x){return(entropy(data[,col],x))}))
}
Dtree = function(data){
  l = list()
  col = ncol(data)
  ig = infogain(data)
  cname = names(data)
  node=names(which.max(ig))
  l[[node]]=list()
  t = table(data[c(node,cname[col])])
  entropies = apply(t,1,FUN=entropy)
  for(i in names(entropies)){
    print(i)
    if(entropies[i]==0){
      l[[node]][i]=names(which(t[i,]!=0))
    }
    else{
      print('dtree called')
      l[[node]][[i]]=Dtree(data[data[node]==i,-which(cname==node)])
    }
  }
  return(l)
}