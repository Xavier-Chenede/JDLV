###create baseline grids

library(plot.matrix)

##structures oscillantes de base

        #structure#1
        #grid
        m3 <- matrix(nrow=3, ncol=3)
        #input data
        m3[,2] <- c(1,1,1)
        m3

        
        #structure#2
        #grid
        mX <- matrix(nrow=8, ncol=8)
        #input data
        mX[,1] <- c(NA,NA,1,1,1,1,NA,NA)
        mX[,2] <- c(NA,NA,1,NA,NA,1,NA,NA)
        mX[,3] <-c(1,1,1,NA,NA,1,1,1)
        mX[,4] <- c(1,NA,NA,NA,NA,NA,NA,1)
        mX[,5] <- c(1,NA,NA,NA,NA,NA,NA,1)
        mX[,6] <-c(1,1,1,NA,NA,1,1,1)
        mX[,7] <- c(NA,NA,1,NA,NA,1,NA,NA)
        mX[,8] <- c(NA,NA,1,1,1,1,NA,NA)
        mX

##structures "vaisseaux"
        
        #structure#3
        #grid
        mglider <- matrix(nrow=3, ncol=3)
        #input data
        mglider[,1] <- c(1,NA,1)
        mglider[,2] <- c(NA,1,1)
        mglider[,3] <- c(NA,1,NA)
        mglider
      
        #structure#4
        #grid
        mglider2 <- matrix(nrow=7, ncol=7)
        #input data
        mglider2[,1] <- c(NA,1,NA,NA,NA,NA,NA)
        mglider2[,2] <- c(1,1,NA,NA,NA,NA,NA)
        mglider2[,3] <- c(1,NA,1,NA,NA,NA,NA)
        
        mglider2[,4] <- c(NA,NA,NA,NA,NA,NA,NA)
        
        mglider2[,5] <- c(NA,NA,NA,NA,1,NA,1)
        mglider2[,6] <- c(NA,NA,NA,NA,NA,1,1)
        mglider2[,7] <- c(NA,NA,NA,NA,NA,1,NA)
        mglider2  
        plot(mglider2)
        
## "random" structures
        
        size <- 40
        val <- c(NA,NA,NA,1)
        inp <-  sample(x = val, replace = TRUE, size = size*size)
        rand_mat <- matrix(inp,nrow=size,ncol=size)
        rand_mat
        plot(rand_mat, key=NULL)
     
        size <- 40
        val <- c(NA,NA,1,1)
        inp <-  sample(x = val, replace = TRUE, size = size*size/2)
        rand_mat1 <- matrix(inp,nrow=size/2,ncol=size)
        rand_mat1
        plot(rand_mat1, key=NULL)

        