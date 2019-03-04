# Read data set
data = read.csv("price_data.csv")

# Average return calcualtion
cumret = log(data[2:nrow(data),2:ncol(data)])- log(data[1:nrow(data)-1,2:ncol(data)])
R = colMeans(cumret)

Ravg = mean(R)
                          # Without shortselling

#calculate variance- covariance matrix which will be used in calculating portfolio variance

var_cov = cov(cumret)

# use library quadprog to optimize it and construct relevant dumy variables
library(quadprog)

#XA = Y
#construct A matrix
ones = rep(1,(ncol(data)-1))
A = cbind(ones,R)

#construct Y matrix
Y = rbind(1,Ravg)

d = rep(0,(ncol(data)-1))
pf = solve.QP(Dmat= var_cov, dvec=d, Amat=(A),bvec = Y ,meq = 1)

optimal_weight = pf$solution

                             # With shortselling

ones = rep(1,ncol(data)-1)
A = cbind(ones,R,diag(1,(ncol(data)-1)))

list = cbind(rep(0,(ncol(data)-1)))
Y = rbind(1,Ravg,list)
Y
pf1 = solve.QP(Dmat = var_cov,dvec =d , Amat =A , bvec = Y ,meq=1)
weights= pf1$solution

#CONSTUCT EFFIECENT PORTFOLIO
c=1
npoint = 100
delta = (max(R)-min(R))/npoint
risk = rep(0,npoint+1)

returnsequence = seq(min(R),max(R),delta)
for(i in returnsequence)
{
  Y = rbind(1,i,cbind(rep(0,(ncol(data)-1))))
  pf = solve.QP(Dmat = var_cov,dvec =d , Amat =A , bvec = Y ,meq=2)
  risk[c] = pf$value
  c= c+1
}
