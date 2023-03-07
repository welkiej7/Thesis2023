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




path_1 <- "~/Desktop/test/followers/group_b"
path_2 <- "~/Desktop/test/followers/group_a"
paths <- list(path_1,path_2)

create_groups(paths = paths)

length(paths)
