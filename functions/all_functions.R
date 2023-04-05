collect_followers_all_v2 <- function(user, saveAs, savedFrame = NULL, cursor, to){
    require(tidyverse)
    require(rtweet)
    if(is.null(savedFrame)){
        #User information
      followers  <- lookup_users(users = user)$followers_count
      iteration <- ceiling(followers/75000)  
        #Main Frame
       main.frame <- get_followers(user = paste("@",user, sep = ""), 
       retryonratelimit =  TRUE, 
       n = 75000)
        #Inform User
        post_message(text = paste("Starting to the Follower Collection Process of", user, sep = " "), user = to)
        #Rate Reset 
        pbar <- txtProgressBar(min = 0, max = 900, char = "=", style = 3)
        for(seconds in 1:900){
          setTxtProgressBar(pbar, value = seconds)
          Sys.sleep(1)
        }
        cursor.info <- next_cursor(main.frame)
        #Start on For Loop
      for(iter in 1:(iteration-1)){
        #Get Followers
      temp.frame  <- get_followers(user = paste("@",user, sep = ""), 
      retryonratelimit =  TRUE, n = 75000, cursor = cursor.info)
        #Combine Rows and Write Csv
      main.frame<- rbind(temp.frame,main.frame) 
      write.csv(main.frame, saveAs, row.names = FALSE )
        #Inform User
      cursor.info <- next_cursor(main.frame)
      post_message(text = paste("Current Cursor is",cursor.info,"At Iteration",iter,
      "of the user",user, sep = " "), user = to)
         if(cursor.info == "0"){
          message("All followers retrieved")
          break
        }
        #Wait for the Rate Limit Reset
        for(seconds in 1:900){
          setTxtProgressBar(pbar, value = seconds)
          Sys.sleep(1)
        }      
      }
    } else {
      ## Read Already Existing File and Calculate the Necessary Time
     main.frame <- readr::read_csv(savedFrame) 
     main.frame.rows <-  nrow(savedFrame)
    followers  <- lookup_users(users = user)$followers_count
    iteration <- ceiling((followers - main.frame.rows)/75000)
    post_message(text = paste("Leftover Iteration of",user,"running.", sep = " "), user = to)
    cursor.info <- cursor
    for(iter in 1:iteration){
    temp.frame <-   get_followers(user = paste("@",user,sep = ""), n = 75000, retryonratelimit = TRUE, cursor = cursor.info) 
    main.frame <- rbind(temp.frame,main.frame)
    cursor.info <- next_cursor(temp.frame)
    write.csv(main.frame, saveAs, row.names = FALSE)
    post_message(text = paste("LeftIt:",user,"Cursor:",cursor.info,"At",iter,"of",iteration,sep = " "), user = to)
    pbar <- txtProgressBar(min = 0, max = 900, style =  3, char = "=")
    if(cursor.info == "0"){
      message("All followers retrieved")
      break
    }
    for(seconds in 1:900){
      setTxtProgressBar(pbar, value = seconds)
      Sys.sleep(1)
    }
    }
    }
}
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
     network_file.mentions.graph)
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
create_groups <- function(paths){
  ##For this function to work you have to divide the followers in to two groups as folders. function accepts the folder paths
  ##and returns the followers from all followers list as a one group. After that you can use this on political stance function. 
require(tidyverse)
main.followers <- paths
    for(folder in 1:length(paths)){
        files <- list.files(paths[[folder]])
        print(files)
            for(file in 1:length(files)){
           temp.file <- readr::read_csv(paste(paths[[folder]],"/",files[file],sep = ""))
           temp.followers <- temp.file%>%select(c("from_id"))
           main.followers[[folder]][file] <- temp.followers
        } 
    }
  names(main.followers) <- paths
  main.followers <- reshape2::melt(main.followers)
  group.types <- unique(main.followers$L1)
  for(type in 1:length(group.types)){
    assign(paste("group_",type,sep = ""),main.followers%>%filter(main.followers$L1 %in% 
    group.types[type]))
    
    temp <- eval(parse(text = paste("group_",type, sep = "")))
    assign(paste("group_",type,sep = ""), temp[,1], envir = .GlobalEnv)
    
  }
}
graph_properties <- function(graph.list){
    require(tidyverse)
 

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
}
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
graph_properties <- function(graph.list, homophily = FALSE, groups = NULL, decompose = TRUE){
    require(tidyverse)
   if(decompose == TRUE){
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
      political_stance(graph.list.unpacked[[graph]], groups = groups) -> temp.ps.score
         temp.ps.score$Score <- temp.ps.score[,2] - temp.ps.score[,3]
         temp.ps.score%>%mutate(Score = case_when(Score < 0 ~ "Opposition",
                                                   Score > 0 ~ "Incumbent",
                                                   Score == 0 ~ "Neutral")) -> temp.ps.score
      V(graph.list.unpacked[[graph]])$Score <- as.factor(temp.ps.score$Score)

      graph.properties[graph,9] <- igraph::assortativity(graph.list.unpacked[[graph]], types1 = V(graph.list.unpacked[[graph]])$Score)
      message(paste("Graph No:",graph,"completed."))
    }
    }
    message("All Completed.")
    return(graph.properties)
     

      
   }
} else if(decompose == FALSE){
if(homophily == FALSE){

    
    
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
    } else if(homophily == TRUE){
      message("Calculation Includes Homophily Measures. This may take a while.")
   

      graph.properties <- as.data.frame(matrix(NA_real_, nrow = length(graph.list), ncol = 9))

      colnames(graph.properties) <- c("Avg_Path_Length","Avg_Local_Efficiency",
                                    "Centralization_Degree","Centralization_Betweenness",
                                    "Number_of_Components","Mean_Cumulative_Degree_Distribution","Mean_Ego_Size",
                                    "Farthest_Nodes","Homophily_Gr1","Homophily_Gr2","Homophily_Gr3","Homophily_Gr4")
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


      ##Homophily 
      political_stance(graph.list[[graph]], groups = groups) -> temp.ps.score
         temp.ps.score$Score <- temp.ps.score[,2] - temp.ps.score[,3]
         temp.ps.score%>%mutate(Score = case_when(Score < 0 ~ "Opposition",
                                                   Score > 0 ~ "Incumbent",
                                                   Score == 0 ~ "Neutral")) -> temp.ps.score
      V(graph.list[[graph]])$Score <- as.factor(temp.ps.score$Score)

      graph.properties[graph,9] <- igraph::assortativity(graph.list[[graph]], types1 = V(graph.list[[graph]])$Score)
      message(paste("Graph No:",graph,"completed."))
    }
    }
    message("All Completed.")
    return(graph.properties)
     

      
   }







}
}

#### DEFINED DIFFERENTLY FOR ONE GROUP
f_political_stance <- function(network, group_1, group_2){
  
  V(network)$name -> temp.network.names
  temp.network.names <- as.numeric(temp.network.names)
  political_stance <- as.data.frame(matrix(NA_real_, nrow = length(temp.network.names), ncol = 3))
  political_stance[,1] <- temp.network.names
  
  
  group_1 -> group_1.eval

  message("Completed Groups!")
  
  for (user in 1:length(temp.network.names)) {
    
    political_stance[user,2] <- group_1.eval[match(temp.network.names[user],group_1.eval$x),2]

    message(paste("User No: ", user, "Finished##", sep = " "))
  }
  return(political_stance)
}






