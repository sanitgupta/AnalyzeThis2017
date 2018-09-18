

nn <- neuralnet(f,data=try,hidden=22,linear.output=FALSE)

f<-as.formula(paste(ny[!ny %in% "ny"],collapse="+"))

f<-reformulate(colnames(scaled.data), response=colnames(Y))

maxs <- apply(quarters, 2, max)
mins <- apply(quarters, 2, min)


scaled.data <- as.data.frame(scale(quarters,center = mins, scale = maxs - mins))

nn <- neuralnet(f,data=try,hidden=22,linear.output=FALSE)

nn <- neuralnet(Y1+Y2+Y3 ~ mvar2 + mvar3 + mvar4 + mvar5 + mvar6 + mvar7 + mvar8 + mvar9 + mvar10 + mvar11 + mvar13 + mvar14 + mvar15 + mvar16 + mvar17 + mvar18 + mvar19 + mvar20 + mvar21 + mvar22 + mvar23 + mvar24 + mvar25 + mvar26 + mvar27 + mvar28 + mvar29 + mvar30 + mvar31 + mvar32 + mvar33 + mvar34 + mvar35 + mvar36 + mvar37 + mvar38 + mvar39 + mvar40 + mvar41 + mvar42 + mvar43 + mvar44 + mvar45 + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18,data=try2,hidden=c(22,11),linear.output=FALSE)
pr.nn <- compute(nn,scaled.ldbd)
prnn<-data.frame(pr.nn)

nn <- neuralnet(card ~ mvar2 + mvar3 + mvar4 + mvar5 + mvar6 + mvar7 + 
                  mvar8 + mvar9 + mvar10 + mvar11 + mvar13 + mvar14 + mvar15 + 
                  mvar40 + mvar41 + mvar42 + mvar43 + mvar44 + mvar45 + V1 + 
                  V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + 
                  V13 + V14 + V15 + V16 + V17 + V18 + A1 + A2 + A3 + A4 + A5 + 
                  A6,data=trainingData,stepmax=1e6,hidden=24,linear.output=FALSE)

nn <- neuralnet(mvar49+mvar50+mvar51 ~ mvar2 + mvar3 + mvar4 + mvar5 + mvar6 + mvar7 + 
                  mvar8 + mvar9 + mvar10 + mvar11 + mvar13 + mvar14 + mvar15 + 
                  mvar40 + mvar41 + mvar42 + mvar43 + mvar44 + mvar45 + V1 + 
                  V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + 
                  V13 + V14 + V15 + V16 + V17 + V18 + A1 + A2 + A3 + A4 + A5 + 
                  A6,data=trainingData,stepmax=1e6,hidden=24,linear.output=FALSE)

ZZZ<-ZZZ[order(-(ZZZ$X4+ZZZ$X2+ZZZ$X3)),]

ccc <- data.frame(matrix(ncol = 1, nrow = 10000))

for (i in 1:10000)
{
  max=0
  if(ZZZ$X2[i]>max)
  {
  ccc[i,1]<-"Supp"
  max<-ZZZ$X2[i]
  }
    
  if(ZZZ$X3[i]>max)
  {
    ccc[i,1]<-"Elite"
    max<-ZZZ$X3[i]  
  }
  
  if(ZZZ$X4[i]>max)
  {
   ccc[i,1]<-"Credit"
   max<-ZZZ$X4[i] 
  }
}

ZZZ2<-cbind(ZZZ$X1,ccc)
card ~ mvar2 + mvar3 + mvar4 + mvar5 + mvar6 + mvar7 + 
  mvar8 + mvar9 + mvar10 + mvar11 + mvar13 + mvar14 + mvar15 + 
  mvar40 + mvar41 + mvar42 + mvar43 + mvar44 + mvar45 + V1 + 
  V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + 
  V13 + V14 + V15 + V16 + V17 + V18 + A1 + A2 + A3 + A4 + A5 + 
  A6

for (i in 1:40000)
{
  if(!(is.na(A$X1)[i]&is.na(A$X2)[i]&is.na(A$X3)[i]))
  {
    if(!is.na(A$X1)[i]&A$X1[i]!=0)
    {
      A$X2[i]<-0
      A$X3[i]<-0
    } 
    if(!is.na(A$X2)[i]&A$X2[i]!=0)
    {
      A$X3[i]<-0
      A$X1[i]<-0
    } 
    if(!is.na(A$X3)[i]&A$X3[i]!=0)
    {
      A$X1[i]<-0
      A$X2[i]<-0
    } 
    A$X4[i]<-0
  }
  else
  {
    if(T$mvar46[i]==1)
      A$X1[i]<-0
    if(T$mvar47[i]==1)
      A$X2[i]<-0
    if(T$mvar48[i]==1)
      A$X3[i]<-0
  }
}

for(i in 1:40000)
{
  if(trd$mvar49[i]==1|trd$mvar50[i]==1|trd$mvar51[i]==1)
  {
    
  }
  else 
  {
    
    if(T$mvar46[i]==1)
    {
      trd$mvar49[i]=0
      trd$mvar50[i]=0.1
      trd$mvar51[i]=0.1
    }
    if(T$mvar47[i]==1)
    {
      trd$mvar49[i]=0.1
      trd$mvar50[i]=0
      trd$mvar51[i]=0.1
    }
    if(T$mvar48[i]==1)
    {
      trd$mvar49[i]=0.1
      trd$mvar50[i]=0.1
      trd$mvar51[i]=0
    }
  }
}

for(i in 1:40000)
{
  if(trd$mvar49[i]==1|trd$mvar50[i]==1|trd$mvar51[i]==1)
  {
    
  }
  else 
  {
    
    if(T$mvar46[i]==1)
    {
      trd$mvar49[i]=0
      trd$mvar50[i]=1
      trd$mvar51[i]=1
    }
    if(T$mvar47[i]==1)
    {
      trd$mvar49[i]=1
      trd$mvar50[i]=0
      trd$mvar51[i]=1
    }
    if(T$mvar48[i]==1)
    {
      trd$mvar49[i]=1
      trd$mvar50[i]=1
      trd$mvar51[i]=0
    }
  }
}

saveRDS(nncd7,"nncd2.RDS")
nncd8<-compute(nncd7,scaled.ldbd)
sub7<-sub[order(-nncd8$net.result),]
sub7<-sub7[1:1000,]
write_csv(sub7,"sub7.csv")
nn2 <- neuralnet(mvar49+mvar50+mvar51 ~ mvar2 + mvar3 + mvar4 + mvar5 + mvar6 + mvar7 + mvar8 + mvar9 + mvar10 + mvar11 + mvar13 + mvar14 + mvar15 + mvar40 + mvar41 + mvar42 + mvar43 + mvar44 + mvar45 + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14 + V15 + V16 + V17 + V18 + A1 + A2 + A3 + A4 + A5 + A6,data=trnd,stepmax=1e6,hidden=3,linear.output=FALSE)

saveRDS(nn2,"nns16.RDS")
nn1<-compute(nn2,finalDataset)
nn1<-data.frame(nn1)
ccc <- data.frame(matrix(ncol = 1, nrow = 10000))

for (i in 1:10000)
{
  max=0
  if(nn1$net.result.1[i]>max)
  {
    ccc[i,1]<-"Supp"
    max<-nn1$net.result.1[i]
  }
  
  if(nn1$net.result.2[i]>max)
  {
    ccc[i,1]<-"Elite"
    max<-nn1$net.result.2[i]  
  }
  
  if(nn1$net.result.3[i]>max)
  {
    ccc[i,1]<-"Credit"
    max<-nn1$net.result.1[i] 
  }
}

sub<-cbind(cmf,ccc)
nncd10<-compute(nncd3,finalDataset)
sub11<-sub[order(-nncd10$net.result),]
sub11<-sub11[1:1000,]
write_csv(sub11,"WightWalkers_IITBombay.csv")