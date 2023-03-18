graph_properties <- function(graph.list, homophily = FALSE, groups = NULL){
    require(tidyverse)
 
   if(homophily == FALSE){
    graph.list.unpacked <- list()

    for(graph in 1:length(graph.list)){
       temp.decomposed <- decompose(graph.list[[graph]])
       append(graph.list.unpacked,temp.decomposed) -> graph.list.unpacked
    }
    
    graph.properties <- as.data.frame(matrix(NA_real_,
                                            nrow = length(graph.list.unpacked),
                                            ncol = 8))

    colnames(graph.properties) <- c("Avg_Path_Length","Avg_Local_Efficiency",
                                    "Centralization_Degree","Centralization_Betweenness",
                                    "Number_of_Components","Mean_Cumulative_Degree_Distribution","Mean_Ego_Size",
                                    "Farthest_Nodes")
    

    for(graph in 1:length(graph.list.unpacked)){
     if(is.na(diameter(graph.list.unpacked[[graph]]))){
        next()
     } else {   
      graph.properties[graph,1]  <- igraph::average.path.length(graph.list.unpacked[[graph]])
      graph.properties[graph,2] <- igraph::average_local_efficiency(graph.list.unpacked[[graph]])
      graph.properties[graph,3] <- igraph::centralization.degree(graph.list.unpacked[[graph]])$centralization
      graph.properties[graph,4] <- igraph::centralization.betweenness(graph.list.unpacked[[graph]])$centralization
      graph.properties[graph,5] <- igraph::count_components(graph.list.unpacked[[graph]])
      graph.properties[graph,6]  <-mean(igraph::degree.distribution(graph.list.unpacked[[graph]], cumulative = TRUE)) 
      graph.properties[graph,7] <- mean(igraph::ego_size(graph.list.unpacked[[graph]]))
      graph.properties[graph,8] <- igraph::diameter(graph.list.unpacked[[graph]])
      message(paste("Graph No:",graph,"completed."))
    }
    }
    message("All Completed.")
    return(graph.properties)
   } else if(homophily == TRUE){
      message("Calculation Includes Homophily Measures. This may take a while.")
      graph.list.unpacked <- list()

       for(graph in 1:length(graph.list)){
       temp.decomposed <- decompose(graph.list[[graph]])
       append(graph.list.unpacked,temp.decomposed) -> graph.list.unpacked
      }

      graph.properties <- as.data.frame(matrix(NA_real_, nrow = length(graph.list.unpacked), ncol = 9))

      colnames(graph.properties) <- c("Avg_Path_Length","Avg_Local_Efficiency",
                                    "Centralization_Degree","Centralization_Betweenness",
                                    "Number_of_Components","Mean_Cumulative_Degree_Distribution","Mean_Ego_Size",
                                    "Farthest_Nodes","Homophily")
      for(graph in 1:length(graph.list.unpacked)){
      if(is.na(diameter(graph.list.unpacked[[graph]]))){
        next()
      } else {   
      graph.properties[graph,1]  <- igraph::average.path.length(graph.list.unpacked[[graph]])
      graph.properties[graph,2] <- igraph::average_local_efficiency(graph.list.unpacked[[graph]])
      graph.properties[graph,3] <- igraph::centralization.degree(graph.list.unpacked[[graph]])$centralization
      graph.properties[graph,4] <- igraph::centralization.betweenness(graph.list.unpacked[[graph]])$centralization
      graph.properties[graph,5] <- igraph::count_components(graph.list.unpacked[[graph]])
      graph.properties[graph,6]  <-mean(igraph::degree.distribution(graph.list.unpacked[[graph]], cumulative = TRUE)) 
      graph.properties[graph,7] <- mean(igraph::ego_size(graph.list.unpacked[[graph]]))
      graph.properties[graph,8] <- igraph::diameter(graph.list.unpacked[[graph]])


      ##Homophily 
      political_stance(graph, groups = groups) -> temp.ps.score
         temp.ps.score$Score <- temp.ps.score[,2] - temp.ps.score[,3]
         temp.ps.score%>%mutate(Score = case_when(Score < 0 ~ "Opposition",
                                                   Score > 0 ~ "Incumbent",
                                                   Score == 0 ~ "Neutral")) -> temp.ps.score
      V(graph)$Score <- as.factor(temp.ps.score$Score)

      graph.properties[graph,9] <- igraph::assortativity(graph, types1 = V(graph)$Score)
      message(paste("Graph No:",graph,"completed."))
    }
    }
    message("All Completed.")
    return(graph.properties)
     

      
   }
}
