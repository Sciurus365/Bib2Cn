Bib2Cn <- function(file_in, file_out){
  if(! "tidyverse" %in% installed.packages()){
    install.packages("tidyverse")
  }
  library(tidyverse)
  temp <- readLines(file_in, encoding = "UTF-8")
  temp[str_detect(temp, "author.*[\u4E00-\u9FA5]")] <- 
    str_replace_all(
      temp[str_detect(temp, "author.*[\u4E00-\u9FA5]")],
      c(",\\ " = "\\{, \\}",
        "\\ \\\\\\%A\\}" = "\\}",
        "\\ \\\\\\%A\\ " = "\\{, \\}",
        "\\ and\\ " = "\\{, \\}"
        )
    )
  
  for(i in 1:length(temp)){
    if(str_detect(temp[i], "@")){
      j <- i
      
      while(1 == 1){
        if(str_detect(temp[j], "author")){
          if(str_detect(temp[j], "[\u4E00-\u9FA5]+")){
            temp[i] <- str_replace(temp[i], "_", str_extract(temp[j], "[\u4E00-\u9FA5]+"))
          }
          break()
        }
        j <- j+1
      }
      
    }
  }
  
  write.table(temp, 
              file = file_out, row.names = F, quote = F, sep="\n",
              fileEncoding = "UTF-8")
}


