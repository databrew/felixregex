library(stringr)
library(dplyr)
input <- c("Klacid Filmtabl 500 mg (Clarithromycin)",
           "Co-Amoxi Mepha Trockensub 1'200 mg (1.2 g) (Amoxicillin, Clavulansäure)",
           "Benerva Tabl 300 mg (Thiamin (Vitamin B1))",
           "Vi-De 3 Tropfen (100 E = 1 gtt) 4'500 E/1 ml (Colecalciferol (Vitamin D3))")


find_med <- function(string){
  
  # Split at parentheses starts
  first_split <- unlist(strsplit(x = string, split = ' (', fixed = T))
  
  # Count the number of parentheses closures on the last object
  n <- length(unlist(gregexpr(")", first_split[length(first_split)])))
  
  # Get the element at length of first_split minus (n-1)
  out_index <- length(first_split) - (n-1)
  out <- as.character(first_split[out_index])
  # Remove any left over trailing parentheses
  out <- str_remove(out, '\\)')
  return(out)
}
# Vectorize
find_med <- Vectorize(find_med)

# Run
result <- find_med(input)

# Test
expected <- c("Clarithromycin", "Amoxicillin, Clavulansäure", "Thiamin", "Colecalciferol")

test_result <- all(result == expected)
if(test_result){
  message('That worked')
} else {
  message('Sorry, Felix.')
}