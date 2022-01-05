# Make a new code
# ##############################################################################
#' Make a new code
#'
#' @param wd Working directory
#' @param fig.out.dir Folder where the pdf output will be saved.
#' @param code.dir.name
#' @param project.name Project name
#' @return
# ---------------------------------------------------------------------------- #
new.code <- function(wd="c:/R-programme/Daten",
                     fig.out.dir="c:/localtexmf/docs/",
                     code.dir.name = "c:/R-Programme/",
                     project.name = "to be changed"){

   cat("",
       "",
       paste0("setwd(\"",wd,"\")"),
       "",
       paste0("fig.out.dir <- \"",fig.out.dir,"\""),
       paste0("code.dir.name <- \"",code.dir.name,"\""),
       paste0("project.name <- \"",project.name,"\""),
       "",
       "pdf(paste0(fig.out.dir,project.name,",
       "        \"to be changed.pdf\"),width=10,height=6,pointsize=12)",
       "",
       "dev.off()",

       sep="\n"
   )
}
