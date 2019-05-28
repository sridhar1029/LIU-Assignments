library(readxl)
library(ggplot2)

#reading data and setting col names
population <- read_excel("population.xls", 
                         skip = 9, col_names = FALSE, na = '.')

colNm = c("Code", "County Municipality", "Population", "Population growth", 
          "Live Births", "Deaths", "Population surplus", "In_mig_tot", "In_mig_from_sc",
          "In_mig_from_ros", "In_mig_from_ab", "Out_mig_tot", "Out_mig_from_sc", 
          "Out_mig_from_ros", "Out_mig_from_ab", "Net_mig_tot", "Net_mig_from_sc",
          "Net_mig_from_ros", "Net_mig_from_ab", "Adjustments")
colnames(population) = colNm
View(population)

#splitting counties and cities
population$keep = population$Code <= 25
counties = population[population$keep,c("Code", "County Municipality", "Population")]
cities = population[!population$keep,c("Code", "County Municipality", "Population")]

#check
sum(counties$Population)
sum(cities$Population)

#counties population plot
ggplot(counties, aes(counties$`County Municipality` , counties$Population)) + 
  geom_bar(stat = "identity")

ggplot(cities, aes(cities$`County Municipality` , cities$Population)) + 
  geom_bar(stat = "identity")


selectRandCity = function(data){
  total_pop = sum(data$Population)
  data$cumPop = cumsum(data$Population)
  randNum = ceiling(runif(1, 0, total_pop))
  selected_ind = which.max((data$cumPop > randNum )*1)
  return(selected_ind)
}

selectedCities = cities[1,]
for(i in 1:20){
  ind = selectRandCity(cities)
  selectedCities[i,] = cities[ind,]
  cities = cities[-ind,]
}

selectedCities
ggplot(selectedCities, aes(selectedCities$`County Municipality` , selectedCities$Population)) + 
  geom_bar(stat = "identity")
