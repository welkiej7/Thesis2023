graph_properties <- function(graph.list){
    require(tidyverse)
 

   
    
    graph.properties <- as.data.frame(matrix(NA_real_,
                                            nrow = length(graph.list),
                                            ncol = 8))

    colnames(graph.properties) <- c("Avg_Path_Length","Avg_Local_Efficiency",
                                    "Centralization_Degree","Centralization_Betweenness",
                                    "Number_of_Components","Mean_Cumulative_Degree_Distribution","Mean_Ego_Size",
                                    "Farthest_Nodes")
    

    for(graph in 1:length(graph.list)){
     if(is.na(diameter(graph.list[[graph]]))){
        next()
     } else {   
      graph.properties[graph,1]  <- igraph::average.path.length(graph.list[[graph]])
      graph.properties[graph,2] <- igraph::average_local_efficiency(graph.list[[graph]])
      graph.properties[graph,3] <- igraph::centralization.degree(graph.list[[graph]])$centralization
      graph.properties[graph,4] <- igraph::centralization.betweenness(graph.list[[graph]])$centralization
      graph.properties[graph,5] <- igraph::count_components(graph.list[[graph]])
      graph.properties[graph,6]  <-mean(igraph::degree.distribution(graph.list[[graph]], cumulative = TRUE)) 
      graph.properties[graph,7] <- mean(igraph::ego_size(graph.list[[graph]]))
      graph.properties[graph,8] <- igraph::diameter(graph.list[[graph]])
      message(paste("Graph No:",graph,"completed."))
    }
    }
    message("All Completed.")
    return(graph.properties)
}
