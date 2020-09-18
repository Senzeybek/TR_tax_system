output_path <- paste("Senaryo sonuclari", Sys.time(), sep = "_")
output_path <- str_replace_all(output_path,":","-")
output_path <- paste("r_output/", output_path, sep = "")
dir.create(output_path)



data_result_path <- paste(output_path,"data",sep="/")
data_result_path <- paste(data_result_path,"xlsx",sep = ".")
export(data,data_result_path)


