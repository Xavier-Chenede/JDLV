# install.packages("ggplot2")
# library(ggplot2)
# install.packages('tidyverse')
library(tidyverse)
# install.packages('gganimate')
library(gganimate)


# initial state
  # data
base_mod <- c(0,0,0,0,0,0,   0,0,0,1,0,0,    0,0,1,1,0,0,    0,0,1,1,0,0,   0,0,1,0,0,0,    0,0,0,0,0,0)
mat0 <- matrix(base_mod,6,6,dimnames=list(1:6,1:6))
(mat0)

  # graph
pre_iteration <- c()
for (i in 6:1) {
  dati <- c(base_mod[c(i,i+6,i+12,i+18,i+24, i+30)])
  pre_iteration <- c(pre_iteration,dati)
}

Iteration <- data.frame(x=rep(1:6,times=6), y=rep(1:6, each=6), z=pre_iteration,ite = rep(0,times=length(pre_iteration)))
(Iteration)

# Main loop for next iteration begins here
for (i in 1:5){

      #to remove edgeline 
    mx <- ncol(mat0)-1 
    my <- nrow(mat0)-1
    new_mat <- mat0
    
      #Calcul 
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
    
       #convert matrix into df for graph purpose
    new_val <- c()
    for (c in 6:1) {
      int_df  <- c(new_mat[c,])
      new_val <-c(new_val,int_df)
    } 
    itex <- data.frame(x=rep(1:6,times=6), y=rep(1:6, each=6),z=new_val, ite=i)
    (itex)  
  
    
      

    #Add data to main dataframe
    
    Iteration <- rbind(Iteration, itex)
    
}# end of the main loop    
    
View (Iteration)


# ====================GGPLOT-ANIM==========================

p <- ggplot(data = Iteration) +
  geom_point(mapping = aes(x = x, 
                           y = y,
                           colour = z,
                           group = ite)) +
  ggtitle("Game of life") +
  xlab("x") + 
  ylab("y") +
  labs(colour = "alive") +
  scale_color_gradient(low = "paleturquoise", 
                       high = "#008080") +
  theme_classic() +
  coord_quickmap()

p
    
panim <- p + 
  transition_states(ite,  wrap = FALSE, 
                    transition_length = 3, 
                    state_length = 0.5)+
  shadow_mark(past = TRUE, 
              future = FALSE) + 
  enter_fade() +
  exit_fade()

panim

animate(panim, nframes = 100, end_pause = 15, rewind = TRUE) 
    



