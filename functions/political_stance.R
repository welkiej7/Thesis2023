political_stance <- function(network, groups){
    require(ggraph)
    require(igraph)
    require(tidyverse)
    political_str <- as.data.frame(matrix(NA_character_, nrow = length(V(network)$name), ncol = length(groups) + 1))
    political_str[,1] <- V(network)$name
    for(group in 1:length(groups)){
        
        for(user in 1:nrow(political_str)){
            political_str[user,group + 1] <- length(which(groups[[group]] == political_str[user,1]))
        }
        }
       
    return(political_str)       
}
  



