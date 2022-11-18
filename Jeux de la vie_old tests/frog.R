# install.packages("ggplot2")
# library(ggplot2)
# install.packages('tidyverse')
library(tidyverse)
# install.packages('gganimate')
library(gganimate)


#### initial state ####

  # data
base_mat <- read_csv2("C:/Users/fr43892k/OneDrive - Sanofi/CSO/R/Mes projets/Jeux de la vie/glider.csv",
                 col_names = TRUE,n_max = 6)

  # matrix conversion 
mat0 <- matrix(data.matrix(base_mat, rownames.force = NA),6,6,dimnames=list(1:6,1:6))
mat0


  #initiate the iteration table with basal state (for graphe purpose)

         #create a vector for df injection
pre_iteration <- as.vector(t(mat0))

        #dataframe creation (x are inverted because order x value in matrix are the opposite of the graphe !)
Iteration <- data.frame(x=rep(6:1,times=6), y=rep(1:6, each=6), z=pre_iteration,ite = rep(0,times=length(pre_iteration)))



# Check view********************************************************
# (Iteration)
# ggplot(data = Iteration) +
# geom_tile(mapping = aes(x = x, y = y,fill = z, group = ite))
# end Check view*****************************************************


#### Generation of further states ####

  # Main loop for next iteration begins here

for (i in 1:5){

      #to remove edgeline 
    mx <- ncol(mat0)-1 
    my <- nrow(mat0)-1
    new_mat <- mat0
    
      #generation of the new matrix (state n+1)
    for (j in 2:mx) {
      for (k in 2:my){
     
        
         if (mat0[j,k]==1) {
            if (sum(mat0[j-1,k-1],mat0[j,k-1],mat0[j+1,k-1],mat0[j-1,k],mat0[j+1,k],mat0[j-1,k+1],mat0[j,k+1],mat0[j+1,k+1] ) %in% c(2,3)) {
                
              new_mat[j,k]=1
            } else {
              new_mat[j,k]=0
            }
           
         } else {
           if (sum(mat0[j-1,k-1],mat0[j,k-1],mat0[j+1,k-1],mat0[j-1,k],mat0[j+1,k],mat0[j-1,k+1],mat0[j,k+1],mat0[j+1,k+1] ) ==3 ) {
             new_mat[j,k]=1
           } else {
             new_mat[j,k]=0
           }
         }
        
      }
    }
    
    (new_mat)
    
    
    
    #create a vector for df injection and that would reprocess for next step
    new_it <- as.vector(t(new_mat))
    
  
   #create the new df to inject
   current_it <- data.frame(x=rep(6:1,times=6), y=rep(1:6, each=6), z=new_it,ite = rep(i,times=length(new_it)))
   (current_it)
   
   
   #combine all iteration in final df
   Iteration <- rbind(Iteration, current_it)
   # reinitialize matrix name to reprocess data at state n+1
   mat0 <- new_mat
   

    
}# end of the main loop    
    
View (Iteration)

# Check view********************************************************
ggplot(data = Iteration) +
  geom_tile(mapping = aes(x = x, y = y,fill = z, group = ite)) +
    facet_wrap(~ite)+


#