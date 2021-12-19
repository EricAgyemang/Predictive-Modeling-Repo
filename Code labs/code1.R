###read data#### 
dd=read.csv("http://my.ilstu.edu/~eric/spring-2019/MAT355/Table%204.3%20Poisson%20regression.csv")

####EDA###
summary(dd)
sd(dd[,1]) 

#####calculate step by step####
x=as.matrix(cbind(1,dd[,2]))   ###create x
y=as.matrix(dd[,1])  ###create y
z=y                   ###create z

#########with initial values#######
b1=7
b2=5
b.old=c(b1,b2)
b.new=c(0,0)

iter=1
while(sum(abs(b.new-b.old))>.00001){
 
   if(iter==1){
    b.old=b.old
  }
  else{
    b.old=b.new
  }
  
w=diag(1/as.vector(x%*%b.old),nrow=dim(x)[1])   ###create w matrix

xtwx=t(x)%*%w%*%x 
xtwz=t(x)%*%w%*%z 

b.new=solve(xtwx)%*%xtwz                   ####compute the new b
iter=iter+1

print(iter)
print(b.new)
}


#######glm by R#######
pos.exm=glm(y~dd[,2],family = poisson(link="identity"))

summary(pos.exm)


