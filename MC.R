df<- read.csv("d:/Projects/MonteCarlo/BankNifty.csv", 1)

df2<-data.frame("Date"=df['Date'], "Price"=df['Close'])
for(i in 2:nrow(df))
{
  df2[i, 'Returns']<-log(df[i, 'Close']/df[i-1, 'Close'])
}
df2<-df2[-c(1),]
u<-mean(df2[,3])
v<-var(df2[,3])
std<-sd(df2[,3])
n=nrow(df2)

paths<-1000
count<-30
risk<-c(ncol=paths)
price<-matrix(0, nrow=(count+1), ncol=paths)
for(i in 1:paths)
{
  price[1,i]<-df2[n,2]
  for(j in 2:(count+1))
  {
    price[j,i]<-price[j-1,i]*exp((u-(v/2))+std*qnorm(runif(1, 0, 1)))
  }
  risk[i]=quantile(price[,i], 0.95)
}
estrisk<-mean(risk)
histrisk<-as.numeric(quantile(df[,5], 0.95))
vrisk<-0.4*estrisk+0.6*histrisk
print('Value at Risk=')
print(vrisk)
matplot(price, main="Nifty Bank Simulation", xlab="Day", ylab="Price",type="l")
