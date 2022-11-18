# library (dplyr)
# library (ggplot2)

# create matrix 3x3, all values = 0
 g <- matrix(0,25,25) 
 
 # create line of 3 inside matrix
 g[13,12]=1
 g[13,13]=1
 g[13,14]=1 
 g
 
 # convert as df
  mat_to_df(x=g)
  View(gg_df)
  
  # convert as plot
  ggplot(data = gg_df) +
    geom_tile(mapping = aes(x = x, y = y)) +
    xlim(0,30) + ylim(0,30)
 
  # df_g0=as.data.frame(g)
 
 next_state_calc(x = g)
 mat_to_df(x=new_mat)
 View(gg_df)
 
 # convert as plot
 ggplot(data = gg_df) +
   geom_tile(mapping = aes(x = x, y = y)) +
   xlim(0,30) + ylim(0,30)
 
 
  # Tidy up df_g for ggplot and geom tile conception
  # dim table
     # determine xmax
   xmax <- ncol(df_g0)
   xmax
    #add reordering variable
   df_g0 <- cbind (df_g0,ordery=c(xmax:1)) %>% arrange(ordery)
   df_g0
  
    # for table length determination
 help_length <-  nrow(df_g0)
 help_length
    #create x coordinates
 xcoor <- rep(1:xmax, times=help_length )
 xcoor
    #create y coordinates
   ycoor <- c()
   df_g1  <-  df_g0 %>% select(-ordery)
   for (i in 1:help_length ){ 
     ycoor0  <- as.numeric(df_g1[i,])
     ycoor <- c(ycoor,ycoor0)
     print(ycoor)
     }
   #generate df for ggplot
   gg_df <- data.frame(x=xcoor, y=ycoor)
   gg_df
 
   
 # convert as plot
 ggplot(data = gg_df) +
   geom_tile(mapping = aes(x = x, y = y)) +
   xlim(0,30) + ylim(0,30)
 
 
# Calcul next state
 
 new_mat <- matrix(rep(0:0, times= (nrow(g)*ncol(g))), nrow=nrow(g), ncol=ncol(g))

 for (j in 2:24) {
   for (k in 2:24){
 
 
 if (g[j,k]==1) {
   
   if (sum(g[j-1,k-1],g[j,k-1],g[j+1,k-1],g[j-1,k],g[j+1,k],g[j-1,k+1],g[j,k+1],g[j+1,k+1] ) %in% c(2,3)) {
     new_mat[j,k]=1
   } else {
     new_mat[j,k]=0
   }        
   #manage death cells
 } else {
   
   if (sum(g[j-1,k-1],g[j,k-1],g[j+1,k-1],g[j-1,k],g[j+1,k],g[j-1,k+1],g[j,k+1],g[j+1,k+1] ) ==3 ) {
     new_mat[j,k]=1
   } else {
     new_mat[j,k]=0
   }
 }
     # ====END CALC====   
   }
 }
 new_mat