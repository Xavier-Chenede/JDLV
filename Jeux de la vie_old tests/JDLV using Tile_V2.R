# install.packages("ggplot2")
# library(ggplot2)
# install.packages('tidyverse')
# library(tidyverse)


# initial state
  # data
dat <- c(0,0,0,0,0,0,   0,0,0,1,0,0,    0,0,1,1,0,0,    0,0,1,1,0,0,   0,0,1,0,0,0,    0,0,0,0,0,0)
mat1 <- matrix(dat,6,6,dimnames=list(1:6,1:6))
(mat1)

  # graph
dat2 <- c()
for (i in 6:1) {
  dati <- c(dat[c(i,i+6,i+12,i+18,i+24, i+30)])
  dat2 <- c(dat2,dati)
}

(dat2)
df <- data.frame(x=rep(1:6,times=6), y=rep(1:6, each=6), z=dat2)
ggplot(df)+     
  aes(x=x, y = y,fill=z)+
  geom_tile()


# Main loop for next steps begins here
for (i in 1:5){

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
    
       #convert matrix into df for graph purpose:
    new_dat <- c()
    for (c in 6:1) {
      int_df <- c(mat2[c,])
      new_dat <-c(new_dat,int_df)
    }
    
    
    # new graph
    df2 <- data.frame(x=rep(1:6,times=6), y=rep(1:6, each=6), z=new_dat)
    lst <- ggplot(df2)+     
      aes(x=x, y = y,fill=z)+
        geom_tile()
    
    
# Set up next loop
    print (i)
    print (mat2)
    mat1 <- mat2 

}# end of the main loop

# force last graphe

print(lst)

