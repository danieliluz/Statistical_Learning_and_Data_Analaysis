library(shiny)
library(dplyr)
library(cluster)
library(shinythemes)

euclid_dist <- function(a, b) {
  dist_mat <- matrix(NA, nrow=dim(a)[1], ncol= dim(b)[1]) #building a matrix the length of our data and with of k 
  for(i in 1:nrow(b)[1]) {
    dist_mat[,i] <- sqrt(rowSums(t(t(a)-b[i,])^2)) #finding the distance bitween all the points and the centers
  }
  dist_mat
}

k_means <- function(data,k = 3,n = 50) {
  #set.seed(13)
  centers <- data[sample(nrow(data), k),] #initiating k random centers
  cluster_list <- vector(n+1, mode="list") #a list of all my clusters
  center_dist <- if (k>1) {
      euclid_dist(data, centers)
        } else {
            matrix(1,dim(data)[1])} # caculating the disstance
  clusters <- apply(center_dist, 1, which.min) #groping each point to the closest center
  cluster_list[[2]] = clusters
  centers <- apply(data, 2, tapply, clusters, mean) #finding the mean of each group of points to itterate the next center

  counter = 1
  
  while((counter <= n) & (identical(cluster_list[[counter]],cluster_list[[counter + 1]]) == FALSE)) { #running the k means n times or till we get the same clustring
    center_dist <- if (k>1) {
      euclid_dist(data, centers)
    } else {
      matrix(1,dim(data)[1])} # caculating the disstance
    clusters <- apply(center_dist, 1, which.min) #groping each point to the closest center
    centers <- apply(data, 2, tapply, clusters, mean) #finding the mean of each group of points to itterate the next center
    counter = counter+1
    cluster_list[[counter+1]] = clusters
  }
  list(clusters = clusters,centers = centers, max_itter = counter) #return a list of the kmean results 
}


med_dat <-  read.delim("gtex.txt ", skip = 2 , row.names = c(1), header = TRUE)
gennames <-  med_dat[,1]
med_dat <-  med_dat[,-1] #Delete string columns

row_sub = apply(med_dat, 1, function(row) all(row ==0 )) #Go through each row and determine if the values are zero
med_dat <-  med_dat[!row_sub,] #deleting rows that are all zeros

med_dat <- t(med_dat) 
med_dat <- as.data.frame(apply(med_dat, 2, as.numeric)) #Change to numeric
med_dat <- as.matrix(scale(med_dat)) #scaling the gins

ui <- fluidPage(theme = shinytheme("superhero"),# Define UI for the app
                titlePanel("K Means App"), # App title
                
                sidebarLayout(
                  sidebarPanel( # Sidebar panel for inputs
                    # Input: Slider for the number clusters and iterations
                    sliderInput(inputId = "k",
                                label = "Number of clusters:",
                                min = 1,
                                max = 15,
                                value = 3),
                    sliderInput(inputId = "n",
                                label = "Number of iterations:",
                                min = 1,
                                max = 300,
                                value = 30)
                    
                  ),
                  
                  mainPanel(
                    plotOutput(outputId = "kmeansPlot") # Output: kmeans plot
                    
                  )
                )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$kmeansPlot <- renderPlot({
    kmean_res  <-  k_means(data = med_dat,k = input$k,n = input$n) 
    pca_res <- prcomp(med_dat)
    pca_res  <-  as.data.frame(pca_res$x)[c(1,2)]
    pca_res  <- scale(pca_res)
    
    par(mar = c(5.1, 4.1, 2, 3))
    plot(pca_res,col = kmean_res$cluster,main = "K Means Plot",pch = 19)
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)


