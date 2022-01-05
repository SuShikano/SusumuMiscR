# copy to Excel
write.excel <- function(tab, ...) write.table( tab, "clipboard-128", sep="\t", row.names=F)
#write.excel(as.data.frame(output))
