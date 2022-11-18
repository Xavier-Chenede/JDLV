# install.packages("ggplot2")
# library(ggplot2)
# install.packages('tidyverse')
# library(tidyverse)


# initial state
  # data
dat <- c(0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0)
mat1 <- matrix(dat,5,5,dimnames=list(1:5,1:5))
(mat1)

  # graph
dat2 <- c()
for (i in 5:1) {
  dati <- c(dat[c(i,i+5,i+10,i+15,i+20)])
  dat2 <- c(dat2,dati)
}

(dat2)
df <- data.frame(x=rep(1:5,times=5), y=rep(1:5, each=5), z=dat2)
ggplot(df)+     
  aes(x=x, y = y,fill=z)+
  geom_tile()





  #to remove edgeline 
mx <- ncol(mat1)-1 
my <- nrow(mat1)-1
mat2 <- mat1

  #Calcul 
for (j in 2:mx) {
  for (k in 2:my){
 
    
     if (mat1[j,k]==1) {
        if (sum(mat1[j-1,k-1],mat1[j,k-1],mat1[j+1,k-1],mat1[j-1,k],mat1[j+1,k],mat1[j-1,k+1],mat1[j,k+1],mat1[j+1,k+1] ) %in% c(2,3)) {
            
          mat2[j,k]=1
        } else {
          mat2[j,k]=0
        }
       
     } else {
       if (sum(mat1[j-1,k-1],mat1[j,k-1],mat1[j+1,k-1],mat1[j-1,k],mat1[j+1,k],mat1[j-1,k+1],mat1[j,k+1],mat1[j+1,k+1] ) ==3 ) {
         mat2[j,k]=1
       } else {
         mat2[j,k]=0
       }
     }
    
  }
}

  # new matrix
(mat1)
(mat2)

  #convert matrix into df for graph purpose:
new_dat <- c()
for (c in 5:1) {
  int_df <- c(mat2[c,])
  new_dat <-c(new_dat,int_df)
}


# new graph
df2 <- data.frame(x=rep(1:5,times=5), y=rep(1:5, each=5), z=new_dat)
ggplot(df2)+     
  aes(x=x, y = y,fill=z)+
    geom_tile()


matz <- mat2 


# other states (loop)=> ça boucle pas !!!

# Loop
for (r in 1:10){



#to remove edgeline 
mx <- ncol(mat2)-1 
my <- nrow(mat2)-1
matx <- matz

#Calcul 
for (j in 2:mx) {
  for (k in 2:my){
    
    
    if (matz[j,k]==1) {
      if (sum(matz[j-1,k-1],matz[j,k-1],matz[j+1,k-1],matz[j-1,k],matz[j+1,k],matz[j-1,k+1],matz[j,k+1],matz[j+1,k+1] ) %in% c(2,3)) {
        
        matx[j,k]=1
      } else {
        matx[j,k]=0
      }
      
    } else {
      if (sum(matz[j-1,k-1],matz[j,k-1],matz[j+1,k-1],matz[j-1,k],matz[j+1,k],matz[j-1,k+1],matz[j,k+1],matz[j+1,k+1] ) ==3 ) {
        matx[j,k]=1
      } else {
        matx[j,k]=0
      }
    }
  }
}

# new matrix

matz <- matx
(matz)
#convert matrix into df for graph purpose:
new_dat <- c()
for (c in 5:1) {
  int_df <- c(matz[c,])
  new_dat <-c(new_dat,int_df)
}


# new graph
df2 <- data.frame(x=rep(1:5,times=5), y=rep(1:5, each=5), z=new_dat)
ggplot(df2)+     
  aes(x=x, y = y,fill=z)+
  geom_tile()


print(r)


} # end loop
