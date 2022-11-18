# JDV my functions


# Next state calucation : enter a matrix, out a new matrix
next_state_calc <- function(x) {
  for (j in 2:24) {
    for (k in 2:24){
      
      #manage alive cells
        if (x[j,k]==1) {
        
        if (sum(x[j-1,k-1],x[j,k-1],x[j+1,k-1],x[j-1,k],x[j+1,k],x[j-1,k+1],x[j,k+1],x[j+1,k+1] ) %in% c(2,3)) {
          new_mat[j,k]=1
        } else {
          new_mat[j,k]=0
        }  
          
        #manage death cells
      } else {
        
        if (sum(x[j-1,k-1],x[j,k-1],x[j+1,k-1],x[j-1,k],x[j+1,k],x[j-1,k+1],x[j,k+1],x[j+1,k+1] ) ==3 ) {
          new_mat[j,k]=1
        } else {
          new_mat[j,k]=0
        }
      }
      # ====END CALC====   
    }
  }
  return(new_mat)
}

next_state_calc(x = g)

# Produced dataset for GGplot  : enter a matrix, out a dataframe


mat_to_df <- function(x) {

    # convert as df
    df_g0=as.data.frame(x)
    
    # Tidy up df_g for ggplot and geom tile conception
    # dim table
    # determine xmax
    xmax <- ncol(df_g0)
    xmax
    #add reordering variable
    df_g0 <- cbind (df_g0,ordery=c(xmax:1)) %>% arrange(ordery)
    # df_g0
    
    # for table length determination
    help_length <-  nrow(df_g0)
    help_length
    #create x coordinates
    xcoor <- rep(1:xmax, times=help_length )
    return(xcoor)
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
    # return(gg_df)
}
mat_to_df(x=g)
mat_to_df(x=new_mat)

View (gg_df)
