
### Mya's data ####
### Logistic regression ####
# coefficients (Mya's from Dzanga)
beta0 <- 2.2889  # intercept # Mya's (Dzanga)
beta1 <- -0.0028 # slope # Mya's (Dzanga)
SE_beta0 <- 0.2635 # Mya's (Dzanga)
SE_beta1 <- 0.0004 # Mya's (Dzanga)
# beta0 t-value = 8.6875
# beta1 t-value = -6.7870
# detection radius from Dzanga = 1010 m (Mya's thesis)
# 50% p(det) = 802 m (Mya's thesis)

# plot logistic regression from Dzanga data
# Create sequence of x values
x <- seq(0, 2000, length=1000)  # range (m)

# Calculate predicted probabilities from coefficients (Dzanga)
p <- 1/(1 + exp(-(beta0 + beta1*x)))

# plot logistic regression (Dzanga)
plot(x, p, type="l", col="blue", lwd=2,
     xlab="X",
     ylab="Probability",
     main="Logistic Regression Curve (Dzanga)",
     ylim=c(0,1))
abline(h=0.5, lty=2, col="red")  # adds reference line at p=0.5
grid()

# add Upper and lower bounds (95% CI)
upper <- 1/(1 + exp(-(beta0 + 1.96*SE_beta0 + (beta1 + 1.96*SE_beta1)*x)))
lower <- 1/(1 + exp(-(beta0 - 1.96*SE_beta0 + (beta1 - 1.96*SE_beta1)*x)))
lines(x, upper, lty=2, col="gray")
lines(x, lower, lty=2, col="gray")


# estimate distance for p(det) = 0.5 (Dzanga)
x_mid <- -beta0/beta1
x_mid # 817m (Dzanga)
# Add point to your existing plot
points(x_mid, 0.5, col="red", pch=16, cex=1.5)
# Add vertical line
abline(v=x_mid, lty=2, col="red")

# p(det) from distance
range <- 817 # from Mya's paper (not sure why it differs from 817m)
p_mid <- 1/(1 + exp(-(beta0 + beta1*range)))
p_mid  # should be very close to 0.5



### Radius ####
x <- 3.22 #area # Mya's Dzanga
EDR_dz <- sqrt(x/pi)
# 1012

ARU_esa <- 10.27/9 # Mya's Kakum detection area per ARU
EDR_kk <- sqrt(ARU_esa/pi) # estimated detection radius for Kakum ()
# 603

# predicted detection function for Mya's Kakum

# New function with same slope but different intercept
# based on EDR from Kakum (603 m) and beta1 value from Dzanga (-0.0028)
p_kakum <- function(r, beta0) {
  1/(1 + exp(-(beta0 + (beta1 * r))))
}

# Function to calculate EDR for a given beta0
edr_calc_kakum <- function(beta0) {
  integrate(function(r) p_kakum(r, beta0), lower=0, upper=10000)$value
}

# to estimate beta0:
target_edr <- EDR

# Function to minimize (difference between calculated and target EDR)
objective <- function(beta0) {
  edr <- integrate(function(r) p_kakum(r, beta0), lower=0, upper=10)$value
  return(abs(edr - target_edr))
}

# Find optimal beta0
optimal <- optimize(objective, interval=c(-5, 5))
new_beta0 <- optimal$minimum

# Print results
print(paste("New β₀:", round(new_beta0, 4)))
print(paste("Dzanga β₀:",beta0))
print(paste("β₁ (unchanged):", -0.0028))

# Verify the EDR with new parameters
edr_new <- integrate(function(r) p_kakum(r, new_beta0), lower=0, upper=10)$value
print(paste("Calculated EDR:", round(edr_new, 2), "meters"))
print(paste("Target EDR:", target_edr, "meters"))

# We can also calculate the ESA to verify
esa_new <- pi * edr_new^2
print(paste("Calculated ESA:", round(esa_new, 4), "km²"))
print(paste("Target ESA:", round(ARU_esa, 4), "km²"))

### estimated kakum detection function from Mya's data ####
# Detection function parameters

# Create plot
x <- seq(0, 2000, length=1000)  # distance in meters
p <- 1/(1 + exp(-(abs(new_beta0) + beta1 * x)))

plot(x, p, type="l", col="blue", lwd=2,
     xlab="Distance (m)",
     ylab="Detection Probability",
     main="Detection Function",
     ylim=c(0,1))
abline(h=0.5, lty=2, col="red")

# Calculate x_mid in meters
x_mid <- (abs(new_beta0)/beta1)
print(paste("Distance where p = 0.5:", round(x_mid, 1), "m"))

# Add vertical line and point at x_mid
abline(v=x_mid, lty=2, col="red")
points(x_mid, 0.5, col="red", pch=16, cex=1.5)
grid()

# Check probabilities at different distances
distances <- c(200, 500, 800, 1000)  # in meters
probs <- 1/(1 + exp(-(new_beta0 + beta1 * distances)))
for(i in 1:length(distances)) {
  print(paste("Probability at", distances[i], "m:", round(probs[i], 3)))
}


#### Buckland - example ####
# http://examples.distancesampling.org/Distance-spec-covar/species-covariate-distill.html
library(Distance)
library(knitr)
birds <- data(Savannah_sparrow_1980)
# Add object column (required for dht2)
birds$object <- NA
birds$object[!is.na(birds$distance)] <- 1:sum(!is.na(birds$distance))


