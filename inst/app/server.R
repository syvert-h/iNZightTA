server <- function(input, output, session){

  # Loop through all .R files in the subfolder "/R" and load them
  
  for (r_file in list.files(path = system.file("app/R", package = "inzightta"), pattern = "\\.R$", full.names = T)) { 
    source(r_file, local = T)$value
  }
  
}