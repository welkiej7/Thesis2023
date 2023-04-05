##Define Graph List


graph.properties <- as.data.frame(matrix(NA_real_, nrow = length(graph.list), ncol = 12))
library(igraph)
colnames(graph.properties) <- c("Avg_Path_Length","Avg_Local_Efficiency",
                                    "Centralization_Degree","Centralization_Betweenness",
                                    "Number_of_Components","Mean_Cumulative_Degree_Distribution","Mean_Ego_Size",
                                    "Farthest_Nodes","Homophily_Gr1","Homophily_Gr2","Homophily_Gr3","Homophily_Gr4")

for(graph in 1:length(graph.list)){
      graph.properties[graph,1]  <- igraph::average.path.length(graph.list[[graph]])
      graph.properties[graph,2] <- igraph::average_local_efficiency(graph.list[[graph]])
      graph.properties[graph,3] <- igraph::centralization.degree(graph.list[[graph]])$centralization
      graph.properties[graph,4] <- igraph::centralization.betweenness(graph.list[[graph]])$centralization
      graph.properties[graph,5] <- igraph::count_components(graph.list[[graph]])
      graph.properties[graph,6]  <-mean(igraph::degree.distribution(graph.list[[graph]], cumulative = TRUE)) 
      graph.properties[graph,7] <- mean(igraph::ego_size(graph.list[[graph]]))
      graph.properties[graph,8] <- igraph::diameter(graph.list[[graph]])
      
      
      
      as.numeric(V(graph.list[[graph]])$name) -> temp.network.names
      political_stance <- as.data.frame(matrix(NA_real_, nrow = length(temp.network.names), ncol = 5))

      political_stance[,1] <- temp.network.names
      for(user in 1:length(temp.network.names)){
        political_stance[user,2] <- group_1[match(temp.network.names[user],group_1$x),2]
        political_stance[user,3] <- group_1[match(temp.network.names[user],group_2$x),2]
        political_stance[user,4] <- group_1[match(temp.network.names[user],group_3$x),2]
        political_stance[user,5] <- group_1[match(temp.network.names[user],group_4$x),2]
        message(paste("Graph",graph,",User",user,"Completed.", sep = ""))
      }
      V(graph.list[[graph]])$GR1_SCORE <- as.factor(political_stance[,2])
      V(graph.list[[graph]])$GR2_SCORE <- as.factor(political_stance[,3])
      V(graph.list[[graph]])$GR3_SCORE <- as.factor(political_stance[,4])
      V(graph.list[[graph]])$GR4_SCORE <- as.factor(political_stance[,5])

      graph.properties[graph,9] <- assortativity_nominal(graph.list[[graph]], types = V(graph.list[[graph]])$GR1_SCORE, directed = FALSE)
      graph.properties[graph,10] <- assortativity_nominal(graph.list[[graph]], types = V(graph.list[[graph]])$GR2_SCORE, directed = FALSE)
      graph.properties[graph,11] <- assortativity_nominal(graph.list[[graph]], types = V(graph.list[[graph]])$GR3_SCORE, directed = FALSE)
      graph.properties[graph,12] <- assortativity_nominal(graph.list[[graph]], types = V(graph.list[[graph]])$GR4_SCORE, directed = FALSE)
      }

    