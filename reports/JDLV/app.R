#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

#______________FUNCTIONS________________

##Input data and setting
#G0 (input data)
d <-rand_mat
# nb gen to compute
n <- 7
#automatic setting

## run generation iterations

expand_gen(d,n)

# Create array combining all matrices

#1 Adjust all the matrices to the same size
adjust(l)

#2 initiate array
aggreg <- array( dim=c(nrow(l[[1]]),ncol(l[[1]]),length(l)))

#3 populate array
for (i in 1:length(l)){
    aggreg[,,i] <- l[[i]]
}



##Plots generations 
require (glue)
#convert array to ggplot compatibility

#1 initialization
df <- data.frame(gen=c(),x=c(),y=c(),z=c())

#2 array z-dimension
z_dim <- c(dim(aggreg))[3]

#3 loop each z of the array



    for(m in 1:z_dim){
        print (glue("generation {m-1} ongoing"))
        mtrx <- aggreg[,,m]
        # mtrx
        g  <- c()
        # x  <- c()
        # y0 <- c()
        y  <- c()
        z  <- c()
        mat_g <- c()
        mat_x <- c() 
        mat_y <- c()
        mat_z <- c()
        
        x <- c(1:ncol(mtrx))
        y0 <- c(nrow(mtrx):1)
        
        colnames(mtrx) <- x
        rownames(mtrx) <- y0
        
        for(j in 1:length(y0)){
            
            for(i in 1:length(x)){
                add_z <- c(mtrx[y0[y0=j],x[i]])
                z <- c(z,add_z)
                y <- c(y,rev(y0)[y0=j])
                g <- c(g,m-1)
            }
            
            mat_x <- c(mat_x,x) 
            mat_y <- c(mat_y,y)
            y <- c()
            mat_z <- c(mat_z,z)
            z <- c()
            mat_g <- c(mat_g,g)
            g <- c()
            
        }
        
        df_mtx <- data.frame(gen=mat_g,x=mat_x, y=mat_y, z=mat_z)
        df_mtx$s <- rep(sum(df_mtx$z, na.rm= TRUE), times= length(df_mtx$z))
        df <- rbind(df,df_mtx)
        df_mtx <- data.frame()
        
    }


#4 Clean up dataframe (remove NA values)
keep_cleaned <- complete.cases(df)
df_c <- df[keep_cleaned,]

#______________END FUNCTIONS________________


# Define UI for application that draws a tile graphes
ui <- fluidPage(

    # Application title
    titlePanel("JEU DE LA VIE"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("gen",
                        "Number of generations:",
                        min = 0,
                        max = n,
                        value = n%/%2), #center the default value
            width=4 
        ),
        
    # Show plots of the new generation compared to the G0
        fluidRow(
            column(width = 4,plotOutput("gen_0")),
            column(width = 8,plotOutput("gen_grid"))
            
        )
    )
)

# Define server logic required to draw the tile graphes
server <- function(input, output) {

    output$gen_0 <- renderPlot({
        
        ggplot(df_c[df_c$gen==0,],(aes(x,y)))+geom_tile(aes(fill=z),show.legend = FALSE,na.rm = TRUE)+
            scale_x_continuous(breaks=seq(0.5,1000.5,by=2),labels = NULL, name = NULL)+
            scale_y_continuous(breaks=seq(0.5,1000.5,by=2),labels = NULL, name = NULL)+
            theme_bw()+
            theme(axis.ticks = element_blank())
    }) 
    
    output$gen_grid <- renderPlot({
    
         ggplot(df_c[df_c$gen==input$gen,],(aes(x,y)))+geom_tile(aes(fill=z),show.legend = FALSE,na.rm = TRUE)+
            scale_x_continuous(breaks=seq(0.5,1000.5,by=2),labels = NULL, name = NULL)+
            scale_y_continuous(breaks=seq(0.5,1000.5,by=2),labels = NULL, name = NULL)+
            theme_bw()+
            theme(axis.ticks = element_blank())
    })  
}



# Run the application 
shinyApp(ui = ui, server = server)
