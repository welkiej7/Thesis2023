political_stance <- function(network,groups){
    ## Political Stance Calculates the Political Score of Network Vertices. 
    ## Input must be two vectors that contains n elements of follower information.
    ## This approach mainly focuses on the unweighted measure. 
    ## It may take a while to calculate the process depending on the groups length, up to 5 - 10 minutes.
   vertex.names <- igraph::V(network)$name
   vertex.names <- as.numeric(vertex.names)
   political_stance_matrix <- as.data.frame(matrix(NA_real_, nrow = length(vertex.names), ncol = length(groups) + 1))
   political_stance_matrix[,1] <- vertex.names

   for(user in 1:length(vertex.names)){
    message(paste("User no", user, "completed."))
    for(group in 1:length(groups)){
      political_stance_matrix[user,group + 1]  <- sum(vertex.names[user] == groups[[group]])
      colnames(political_stance_matrix)[group + 1] <- paste("group",group,sep = "")
    }
   }
   return(political_stance_matrix)
}




