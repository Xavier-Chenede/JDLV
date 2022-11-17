###Function to manage 'JDLV' rules

# Une cellule possède huit voisins, qui sont les cellules adjacentes horizontalement,
# verticalement et diagonalement. À chaque itération, l'état d’une cellule est
# entièrement déterminé par l’état de ses huit cellules voisines, selon les règles
# suivantes :
# 
#
# rules#1=> une cellule vivante possédant deux ou trois cellules voisines vivantes le reste,
# sinon elle meurt
# rules#2=> une cellule vide possédant exactement trois cellules voisines vivantes devient
# vivante (elle naît) ;


#set up packages
require(tidyverse)
require(glue)
require(rlang)


#Function to generate the new generation of cells

newgen <- function (gen_to_run) {
        
      gen_next <-  matrix(nrow=nrow(gen_to_run), ncol=ncol(gen_to_run))

        # rules#1
        for (r in 2:nrow(gen_to_run)-1){
                for (c in 2:ncol(gen_to_run)-1){
                        if (((sum(gen_to_run[r-1,c-1],
                                  gen_to_run[r-1,c],
                                  gen_to_run[r-1,c+1],
                                  gen_to_run[r,c+1],
                                  gen_to_run[r,c-1],
                                  gen_to_run[r+1,c-1],
                                  gen_to_run[r+1,c],
                                  gen_to_run[r+1,c+1],
                                  na.rm=TRUE)%in% c(2,3))) & (!is.na(gen_to_run[r,c]))) {
                                gen_next[r,c]=1

                        }
                }
        }
        
        # rules#2
        for (r in 2:nrow(gen_to_run)-1){
                for (c in 2:ncol(gen_to_run)-1){
                        if (((sum(gen_to_run[r-1,c-1],
                                  gen_to_run[r-1,c],
                                  gen_to_run[r-1,c+1],
                                  gen_to_run[r,c+1],
                                  gen_to_run[r,c-1],
                                  gen_to_run[r+1,c-1],
                                  gen_to_run[r+1,c],
                                  gen_to_run[r+1,c+1],
                                  na.rm=TRUE)==3)) & (is.na(gen_to_run[r,c])))  {
                                gen_next[r,c]=1

                        }
                }
        }
    setup_matrix(gen_next)

}

# regenerations reiteration (retrieving within a list)


expand_gen <- function(gen_0_mx, nb_gen){
  
        gen_0  <<- setup_matrix(gen_0_mx)
        gen_y  <<- gen_0 
        l      <<- list(gen_y)
        
        for (i in 1:nb_gen){
                gen_y <- newgen(gen_y)
                l <<- c(l,list(gen_y)) #list of matrices with new generations
        }

}


#function to adjust all the matrices to the same size
adjust <- function (l){
  
        # Retrieve Matrices Size
        max_l=c()
        
                for (i in 1:length(l)){
                  max_l <- c(max_l, ncol(l[[i]]))
                }
        
        # Correction vector
        corr <- (max(max_l)-max_l)
        corr_v <- corr*0.5
        
        #adjust matrices
                for (i in 1:length(l)){
                  
                  if(corr_v[i] != 0) {
                            col_na <- matrix(rep(NA,times=nrow(l[[i]])*corr_v[i]), nrow=nrow(l[[i]]), ncol=corr_v[i])
                            col_add <- cbind(col_na,l[[i]],col_na)
                            
                            row_na <- matrix(rep(NA,times=ncol(col_add)*corr_v[i]), nrow=corr_v[i], ncol=ncol(col_add))
                            l[[i]] <<- row_add <- rbind(row_na,col_add,row_na)
                  }
        }
}

















