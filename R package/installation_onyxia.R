system("sudo apt update")
system("sudo apt install libprotoc-dev libprotobuf-dev protobuf-compiler")

install.packages(c("zoo", "RProtoBuf", "mathjaxr"))
# devtools::install_github("palatej/rjd3toolkit")
install.packages("R package/rjd3toolkit_0.1.1.tar.gz", repos = NULL, type = "source")
devtools::install_github("AQLT/rjdfilters")
devtools::install_github("AQLT/AQLThesis")