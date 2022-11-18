# install.packages("ggplot2")
# library(ggplot2)
# install.packages('tidyverse')

# library(tidyverse)
# library(gganimate)


#### initial state ####

  # data
base_mat <- read_csv2("C:/Users/fr43892k/OneDrive - Sanofi/CSO/R/Mes projets/Jeux de la vie/inline_10.csv",
                 col_names = TRUE,n_max = 6)

base_mat

  #basal dimension
  nb_x=ncol(base_mat)
  nb_y=nrow(base_mat)
  

  # matrix conversion 
mat0 <- matrix(data.matrix(base_mat, rownames.force = NA),nb_y,nb_x)#,dimnames=list(1:nb_x,1:nb_y))
mat0


  #initiate the iteration table with basal state (for graphe purpose)

         #create a vector for df injection
pre_iteration <- as.vector(t(mat0))

        #dataframe creation (y are inverted because order y value in matrix are the opposite of the graphe !)
Iteration <- data.frame(x=rep(1:nb_x,times=nb_y), y=rep(1:nb_y, each=nb_x), z=pre_iteration,ite = rep(0,times=length(pre_iteration)))



# Check view********************************************************
(mat0)
(Iteration)
ggplot(data = Iteration) +
geom_tile(mapping = aes(x = x, y = y,fill = z, group = ite))+
ggtitle("STATE 0")  

# end Check view*****************************************************

#### Generation of further states ####

  # Main loop start, i is the number of state to generate
for (i in 1:29){
  
  # sum surrounding col/row 
  
  bef_lstx <- ncol(mat0)-1 
  bef_lsty <- nrow(mat0)-1
  
  
  lrs <- sum(mat0[bef_lsty,]) + sum(mat0[nrow(mat0),])
  frs <- sum(mat0[1,])+sum(mat0[2,])
  lcs <- sum(mat0[,bef_lstx]) + sum(mat0[,ncol(mat0)])
  fcs <- sum(mat0[,1])+sum(mat0[,2])

#   print (paste("2 last rows: " , lrs))
#   print (paste("2 first rows: ", frs))
#   print (paste("2 last cols: " , lcs))
#   print (paste("2 first cols: ", fcs))

  if (lrs > 0) {
    mat0 <- rbind(mat0, rep(0:0, times=ncol(mat0)))
    mat0 <- rbind(mat0, rep(0:0, times=ncol(mat0)))  
  }
  
  if (lcs >0) {
    mat0 <- cbind(mat0, rep(0:0, times=nrow(mat0))) 
    mat0 <- cbind(mat0, rep(0:0, times=nrow(mat0)))  
  }
  
  if (frs>0) {
    mat0 <- rbind(rep(0:0, times=ncol(mat0)),mat0)
    mat0 <- rbind(rep(0:0, times=ncol(mat0)),mat0)  
  }
  
  if (fcs >0) {
    mat0 <- cbind(rep(0:0, times=nrow(mat0)),mat0) 
    mat0 <- cbind(rep(0:0, times=nrow(mat0)),mat0)  
  }
  

  # max_j and max_k to exclude edgeline from calculation
  max_j <- nrow(mat0)-1 
  max_k <- ncol(mat0)-1

  # generate empty matrix for new state
  new_mat <- matrix(rep(0:0, times= (nrow(mat0)*ncol(mat0))), nrow=nrow(mat0), ncol=ncol(mat0))
  
  
  # mat0 scanning and calculation of new values in new_mat
  for (j in 2:max_j) {
    for (k in 2:max_k){

    # ====CALC STAT n+1====
      #manage alive cells
      if (mat0[j,k]==1) {
        
        if (sum(mat0[j-1,k-1],mat0[j,k-1],mat0[j+1,k-1],mat0[j-1,k],mat0[j+1,k],mat0[j-1,k+1],mat0[j,k+1],mat0[j+1,k+1] ) %in% c(2,3)) {
          new_mat[j,k]=1
        } else {
          new_mat[j,k]=0
        }
        
      #manage death cells
       } else {
        
        if (sum(mat0[j-1,k-1],mat0[j,k-1],mat0[j+1,k-1],mat0[j-1,k],mat0[j+1,k],mat0[j-1,k+1],mat0[j,k+1],mat0[j+1,k+1] ) ==3 ) {
          new_mat[j,k]=1
        } else {
          new_mat[j,k]=0
        }
      }
    # ====END CALC====
    }
  }
  

  print (paste("round ",i,sep=""))
  

  # reinitialize matrix name to reprocess data at state n+1
  mat0 <- new_mat
  print (new_mat)
  
  #create a vector for df injection and that would reprocess for next step
  new_it <- as.vector(t(new_mat))
  
  #create the new df to inject
    #redim
  nb_y=nrow(new_mat)
  nb_x=ncol(new_mat)
  
  current_it <- data.frame(x=rep(1:nb_x,times=nb_y), y=rep(nb_y:1, each=nb_x), z=new_it,ite = rep(i,times=length(new_it)))

  #combine all iteration in final df
  Iteration <- rbind(Iteration, current_it)
  
  
  # Correction of the coordinate if needed
  
    # print (paste("nb row mat_0 during round ",i,": ",bef_lsty+1,sep=""))
    # print (paste("nb row new_mat 0 during round ",i,": ",nrow(new_mat),sep=""))
    # 
    # print (paste("nb col mat_0 during round ",i,": ",bef_lstx+1,sep=""))
    # print (paste("nb col new_mat 0 during round ",i,": ",ncol(new_mat),sep=""))
  
  diff_row <-  nb_y - (bef_lsty+1)
  diff_col <-  nb_x - (bef_lstx+1)
  if (diff_row>0){Iteration <- Iteration %>% mutate(y2=ifelse(ite<i,y+2,y)) %>% select(x,y2,z,ite) %>% rename("y"="y2")} 
  if (diff_col>0){Iteration <- Iteration %>% mutate(x2=ifelse(ite<i,x+2,x)) %>% select(x2,y,z,ite) %>% rename("x"="x2")} 
  

}  #end main loop


# Check view********************************************************
ggplot(data = Iteration) +
  geom_tile(mapping = aes(x = x, y = y,fill = z, group = ite)) +
  facet_wrap(~ite)






