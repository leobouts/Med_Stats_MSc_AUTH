tremdata<-read.csv(file.choose())
####choose tremdatabasic.csv
####second column is the gold/reference standard
####third and fourth columns trem1 & il6 measurements respectively
tremdata<-tremdata[-47,]

trem2<-tremdata[,2:4]
Gold<-trem2[,1]
Z<-trem2[,2:3]
contrast<-t(c(1,-1))
Zt<-trem2[,2]
Zi<-trem2[,3]

ustat.con<-function(neg,pos)
        {
	  n.neg<-length(neg)
	  n.pos<-length(pos)
	  if (n.neg==1 | n.pos==1){      #labour saving device
	    sum((neg<pos)+0.5*(neg==pos))/(n.neg*n.pos)
	  }
	  else{
	    ranked.cut<-sort(unique(c(neg,pos)))
	    n.ranked<-length(ranked.cut)
	    n.atoms<-n.ranked+1
	    fp<-rep(0,n.atoms)
	    tp<-rep(0,n.atoms)
            for (i in 1:n.ranked){
	      fp[i]<-sum(neg>=ranked.cut[i])/n.neg
	      tp[i]<-sum(pos>=ranked.cut[i])/n.pos
	    }
	    fp.diff<--diff(fp)
	    tp.mean<-(tp[1:(n.atoms-1)] + tp[2:(n.atoms)])/2
	    sum(fp.diff*tp.mean)
	  }
	}



	u.contrast<-function(Gold,Z,contrast)
	{
	  dimen<-dim(Z)[2]
	  Gold.uniq<-unique(sort(Gold))
	  contrast<-as.matrix(contrast)
	  n.neg<-length(Gold[Gold==Gold.uniq[1]])
	  n.pos<-length(Gold)-n.neg
	  Y<-Z[(Gold==Gold.uniq[1]),c(1:dimen)]   # Nondiseased group
	  X<-Z[(Gold==Gold.uniq[2]),c(1:dimen)]   # Diseased group

 
###   Compute all areas (there are dimen number)
	  theta<-rep(0,dimen)
	  for (i in 1:dimen){
	    theta[i]<-ustat.con(Y[,i],X[,i])
          }
	
###  Compute X,Y variance matrices

	  V01<-matrix(0,n.neg,dimen,byrow=T)
	  V10<-matrix(0,n.pos,dimen,byrow=T)
	
	  for (row in 1:n.neg){
	    for (col in 1:dimen){
	      V01[row,col]<-ustat.con(Y[row,col],X[,col])
	    }
	  }

	  for (row in 1:n.pos){
	    for (col in 1:dimen){
	      V10[row,col]<-ustat.con(Y[,col],X[row,col])
	    }
	  }
	

	  S01<-(t(V01)%*%V01-n.neg*outer(theta,theta))/(n.neg-1)
	  S10<-(t(V10)%*%V10-n.pos*outer(theta,theta))/(n.pos-1)
	  Svar<-S10/n.pos+S01/n.neg
	  Var<-contrast%*%Svar%*%t(contrast)
	  test.L<-contrast%*%theta  #Test value
	  test<-t(test.L)%*%solve(Var)%*%test.L #Chisquare value
	  deg.f<-min(dim(contrast))
	  chi.p<-1-pchisq(test,deg.f)


###     Return promised object:

	  return(list(area=theta,testvalue=c(test.L),
                      chisquare=c(test,format(deg.f),chi.p),
                      Var=Var))
	}


u.contrast(Gold,Z,contrast)

library("pROC")

Zt<-trem2[,2]
Zi<-trem2[,3]
roctrem<-roc(Gold,Zt)
rocil6<-roc(Gold,Zi)
roc.test(roctrem,rocil6,method="delong")
roc.test(roctrem,rocil6,method="venkatraman")
roc.test(roctrem,rocil6,method="bootstrap")