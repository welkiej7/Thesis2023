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
