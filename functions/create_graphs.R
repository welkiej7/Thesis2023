create_graphs <- function(network_file, link_type = c("all","retweet","reply","quoted","mentions")){
   require(tidyverse)
   require(ggraph)
   require(igraph)
   type <- match.arg(link_type)


   if(type == "all"){
      message(paste("Selected linkage type is", type , sep = " "))
      network_file <- network_file%>%select(c("user_id",
                                          "reply_to_user_id",
                                          "quoted_user_id",
                                          "retweet_user_id",
                                          "mentions_user_id")) 

      network_file.retweets <- network_file%>%select(c("user_id","retweet_user_id"))
      network_file.replies <- network_file%>%select(c("user_id","reply_to_user_id"))
      network_file.quoted <- network_file%>%select(c("user_id","quoted_user_id"))
      network_file.mentions <- network_file%>%select(c("user_id","mentions_user_id"))

      network_file.retweets <- na.omit(network_file.retweets)
      network_file.replies <- na.omit(network_file.replies)
      network_file.quoted <- na.omit(network_file.quoted)
      network_file.mentions <- na.omit(network_file.mentions)

      network_file.retweets.graph <- graph_from_data_frame(network_file.retweets, directed = TRUE)
      network_file.replies.graph <- graph_from_data_frame(network_file.replies, directed = TRUE)
      network_file.quoted.graph <- graph_from_data_frame(network_file.quoted, directed = TRUE)
      network_file.mentions.graph <- graph_from_data_frame(network_file.mentions, directed = FALSE)

     results <- list(network_file.retweets.graph, network_file.replies.graph, network_file.quoted.graph,
     network_file.mentions)
     names(results) <- c("retweets","replies","quoted","mentions")

     return(results) 

   } else if (type == "retweet") {
      message(paste("Selected linkage type is", link_type, sep = " "))
       network_file.retweets <- network_file%>%select(c("user_id","retweet_user_id"))
       network_file.retweets <- na.omit(network_file.retweets)
       network_file.retweets.graph <- graph_from_data_frame(network_file.retweets, directed = TRUE)
       return(network_file.retweets.graph)
   } else if(type == "reply"){
      message(paste("Selected linkage type is", link_type, sep = " "))
       network_file.replies <- network_file%>%select(c("user_id","reply_to_user_id"))
       network_file.replies <- na.omit(network_file.replies)
       network_file.replies.graph <- graph_from_data_frame(network_file.replies, directed = TRUE)
       return(network_file.replies.graph)
   } else if(type == "quoted"){
       network_file.quoted <- network_file%>%select(c("user_id","quoted"))
       network_file.quoted <- na.omit(network_file.quoted)
       network_file.quoted.graph <- graph_from_data_frame(network_file.quoted, directed = TRUE)
       return(network_file.quoted.graph)
   } else if(type == "mentions"){
      message(paste("Selected linkage type is", link_type, sep = " "))
      network_file.mentions <- network_file%>%select(c("user_id","mentions_user_id"))
      network_file.mentions <- na.omit(network_file.mentions)
      network_file.mentions.graph <- graph_from_data_frame(network_file.mentions, directed = FALSE)
      return(network_file.mentions.graph)
   }
message("Process Completed!")
}