

  wd <- "c:/R-programme/Daten" # In this directory you have to have
                             #     "phantomjs.exe" and "scrape.js".
  setwd(wd)

  output.dir <- "C:/localtexmf/docs/"  # Directory where you like to have html.file
  file.name <- "Bellemare Lagged Explanatory Variables and the Estimation of Causal Effect"
       # Avoid to use "." "?" ":" etc.
  url <- "http://www.journals.uchicago.edu/doi/full/10.1086/690946" # url

  # remove spaces from the file name
  file.name <- gsub(" ","_",file.name)

  # Preparing download
  lines <- readLines("scrape.js")
  lines[1] <- paste0("var url ='", url ,"';")
  writeLines(lines, "scrape.js")

  ## Download website
  system("phantomjs scrape.js",invisible=F)

  ## File rename
  file.rename(paste0(wd,"/1.html"),paste0(output.dir,file.name,".html"))


  #library(mailR)
  #from <- "<susumu.shikano@uni-konstanz.de>"
  #to <- "<shikano_69@kindle.com>"
  #mailControl=list(smtpServer="smtp.uni-konstanz.de")
  #body <- list("",mime_part(paste0(wd,"/",file.name,".html")))
  #sendmail(from=from,to=to,subject=NULL,msg=body,control=mailControl)



