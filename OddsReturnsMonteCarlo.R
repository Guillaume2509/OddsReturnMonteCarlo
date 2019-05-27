##Test your own NPV entries
#Insert your probabilities
probs <- c(0.7, 0.69, 0.57, 0.82, 0.47, 0.9, 0.66, 0.5, 0.46, 0.71, 0.7, 0.69, 0.57, 0.82, 0.47, 0.9, 0.66, 0.5, 0.46, 0.71)
#Insert your respective gains
mult <- c(1.47, 1.47, 1.83, 1.33, 2.4, 1.16, 1.55, 2.6, 2.2, 1.43, 1.47, 1.47, 1.83, 1.33, 2.4, 1.16, 1.55, 2.6, 2.2, 1.43)

#Fonction NPV
NPV <- function(probs, mult) {
  output <- vector(mode = "numeric", length = length(probs))
  for (i in 1:length(probs))  
  if (probs[i] > runif(1))
      output[i] <- mult[i] - 1
    else
      output[i] <- -1
  return (output)
}

#Somme des gains
Somme <- sum(NPV(probs, mult))

#Cycle d'itération
iter <- 100000
vector <- vector(mode = "numeric", length = iter)
for (i in 1:iter)
  vector[i] <- sum(NPV(probs, mult))/length(probs)

#Histogramme
library(ggplot2)
library(scales)
vectorDF <- data.frame(vector)
ggplot(vectorDF, aes(x = vector)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.01, col = "purple", fill = "purple", alpha = 0.5) +
  labs(title = "Retour en pourcentage en fonction de la probabilité", x = "Retour", y = "Probabilité") +
  scale_x_continuous(labels = percent)

#Probabilité de perdre de l'argent
sum(vector < 0)/iter

#Probabilité de perdre plus de 50%
sum(vector < -0.50)/iter

#Probabilité de perdre 100%
sum(vector = -1)/iter

#Probabilité de gagner plus de 50%
sum(vector > 0.50)/iter

#Probabilité de gagner plus de 100%
sum(vector > 1)/iter

#Moyenne de retour
mean(vector)

#Écart-type de retour
sd(vector)

#Score Z = (x - mean(vector))/sd(vector)