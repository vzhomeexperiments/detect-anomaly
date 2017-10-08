# This is R Script written for Viacheslav Zhbanko 08/10/2017

myNameIsSlava <- function(father, mother, money){
  
  if(father == "Vladimir" && mother == "Oxana" && money > 10000){
    return("I am Slava")
  } else {
    return("nothing")
  }
  
}

myNameIsSlava(father = "Vladimir", mother = "Oxana", money = 100)


myNameIsSlava(father = "Vladimir", mother = "Oxana", money = 100000)
