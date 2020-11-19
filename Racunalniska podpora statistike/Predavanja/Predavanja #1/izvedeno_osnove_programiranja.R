
# ------------------------------------------------
# faktor
af1 = c(0,0,0,0,1,1,1,1) # 4 men, 4 women, numeric
af2 = as.factor(af1)
af2
as.numeric(af2) # factor  starts with 1, levels sorted
af3 = factor(af1,levels = c(0,1),labels=c("M","F")) # ordered=TRUE for ordered factors
af3[af3!="M"] # se vedno ohranimo moznost za vrednost "M"
af4 = factor(af3,levels=c("M","F","O")) # dodamo kategorijo "O" (otrok)


# ------------------------------------------------
# matrika, funkcija solve
Y = matrix(runif(16,0,10),ncol=4)
solve(Y,Y)
round(solve(Y,Y)) # zaokrozi na 0 decimalk natancno


# ------------------------------------------------
# podatki z USArrests (> 75% urbane populacije kot nova spremenljivka - dihotomka)
# VARIANTA 1
USArrests$Urban2 = rep("",length(USArrests$UrbanPop))
vrstica = 0
for (i in USArrests$UrbanPop){ 
  vrstica = vrstica + 1
  if (i > 75){
    USArrests$Urban2[vrstica] = "yes"
  }else{
    USArrests$Urban2[vrstica] = "no"
  }
}

# VARIANTA 2
USArrests$Urban2 = rep("",length(USArrests$UrbanPop))
for (i in 1:length(USArrests$UrbanPop)){ # i je indeks!
  if (USArrests$UrbanPop[i] > 75){
    USArrests$Urban2[i] = "yes"
  }else{
    USArrests$Urban2[i] = "no"
  }
}

# VARIANTA 3
tmp = NULL
for (i in 1:length(USArrests$UrbanPop)){ # i je indeks!
  if (USArrests$UrbanPop[i] > 75){
    tmp = c(tmp,"yes")  # append(tmp,"yes")
  }else{
    tmp = c(tmp,"no")
  }
}
USArrests$Urban3 = tmp

#
# vsota in povprecje prvih 100 stevil brez uporabe funkcij v R
vsota = 0
for(i in 1:100){
  vsota = vsota  + i
  povpr = vsota/i
}
vsota/length(1:100)
# FUNKCIJI V R za izracun iste stvari:
sum(1:100)
mean(1:100)


# ------------------------------------------------
# Converts Kelvin to Celsius
# This function converts input temperatures in Kelvin to Celsius.
kelvin_to_celsius <- function(temp_K) {
  temp_C <- temp_K - 273.15
  return(temp_C)
}

# This function converts input temperatures in Celsius to Fahrenheit.
celsius_to_fahrenheit <- function(temp_C) {
  temp_F <- (temp_C * 9/5) + 32
  temp_F
}

# This function converts input temperatures in Kelvin to Fahrenheit.
kelvin_to_fahrenheit <- function(temp_K) {
  temp_C <- kelvin_to_celsius(temp_K)
  temp_F <- celsius_to_fahrenheit(temp_C)
  temp_F
}