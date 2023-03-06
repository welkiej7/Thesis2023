create_groups <- function(paths){
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
    
  return(main.followers)
##Check Melt from Reshape Package
}