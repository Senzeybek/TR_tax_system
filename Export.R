output_path <- paste("Senaryo sonuclari", Sys.time(), sep = "_")
output_path <- str_replace_all(output_path,":","-")
output_path <- paste("r_output/", output_path, sep = "")
dir.create(output_path)

result_files <- c( 
  "data",
  "odd_agustos")


result_path <- paste(output_path,result_files,sep="/")
result_path <- paste(result_files,"xlsx",sep = ".")
lapply(result_files,export)


