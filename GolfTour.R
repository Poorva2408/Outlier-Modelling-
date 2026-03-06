
# Season statistics for individual golfers on US LPGA & PGA Tours  

# https://users.stat.ufl.edu/~winner/data/pgalpga2008.dat

GolfTour=read.table("G:\\PythonS\\R_stats\\GolfTour.txt",header=FALSE)
colnames(GolfTour) = c("Avg Dist", "Drive Accuracy(%)", "Gender")
# Prepend the index so it is column 1
GolfTour <- cbind(Index = 1:nrow(GolfTour), GolfTour)

head(GolfTour)
summary(GolfTour)

# LPGA Stats (Gender(Female) = 1)

datF <- subset(GolfTour, Gender==1, select=1:3)
print(datF)

pairs(datF, c("Avg Dist", "Drive Accuracy(%)"))

# PGA Stats
datM <- subset(GolfTour, Gender==2, select=1:3)
print(datM)

#pairs(datM, c("Avg Dist", "Drive Accuracy(%)"))

#pairs(GolfTour, c("Avg Dist", "Drive Accuracy(%)", "Gender"))

# Scatterplot Female
# Generate the plot
par(mfrow = c(1, 2))

LPGA = plot(datF$`Avg Dist`, datF$`Drive Accuracy(%)`, 
     main = "US LPGA Tour Female (Avg Dist vs Drive Accuracy(%))",
     xlab = "Average Distance (Yards)", 
     ylab = "% Drive Accuracy",
     pch = 19, col = "darkblue") +

# Add a trend line to see the local system behavior
      abline(lm(`Drive Accuracy(%)` ~ `Avg Dist` , data = datF), col = "red", lwd = 2)


# PGA Stats (Gender(male) = 2)

# Male 
plot(datM$`Avg Dist`, datM$`Drive Accuracy(%)`, 
     main = "US PGA Tour Males (Avg Dist vs Drive Accuracy(%))",
     xlab = "Average Distance (Yards)", 
     ylab = "% Drive Accuracy",
     pch = 19, col = "cyan") 

# Add a trend line to see the local system behavior
    abline(lm(`Drive Accuracy(%)` ~ `Avg Dist` , data = datM), col = "red", lwd = 2)
    
par(mfrow = c(1, 1))
########################################################################################
############ PGA + LPGA 
# Overlaying them 

# 1. Create the base plot using the first dataset (LPGA/datF)
# Note: Use backticks for column names with spaces or special characters
plot(datF$`Avg Dist`, datF$`Drive Accuracy(%)`, 
     main = "US LPGA vs PGA Tour Stats",
     xlab = "Average Distance (Yards)", 
     ylab = "% Drive Accuracy",
     pch = 19, col = "darkblue",
     xlim = range(GolfTour$`Avg Dist`), 
     ylim = range(GolfTour$`Drive Accuracy(%)`))

# 2. Add the first trend line (LPGA)
abline(lm(`Drive Accuracy(%)` ~ `Avg Dist`, data = datF), col = "red", lwd = 2)

# 3. Overlay the second dataset (PGA/datM)
# Ensure x is 'Avg Dist' and y is 'Drive Accuracy(%)' to match the axes
points(datM$`Avg Dist`, datM$`Drive Accuracy(%)`, col = "black", pch = 25)

# 4. Add the second trend line (PGA)
abline(lm(`Drive Accuracy(%)` ~ `Avg Dist`, data = datM), col = "orange", lwd = 2)

# 5. Optional: Add a legend to make the system readable
legend("topright", legend = c("LPGA", "PGA"), 
       col = c("darkblue", "black"), pch = c(19, 25))

############################################################################################################################
### LR LPGA 
LPGA_fit = lm(`Drive Accuracy(%)` ~ `Avg Dist`, data = datF)
summary(LPGA_fit)

# Drive Accuracy (%) = 130.89 - (0.256 * Avg Dist)

# Assuming you have the rstanarm package loaded

library(rstanarm)
bayesian_golf <- stan_glm(`Drive Accuracy(%)` ~ `Avg Dist`, data = datF)

# Extract the Posterior Mean
post_mean_slope <- coef(bayesian_golf)["`Avg Dist`"]
print(post_mean_slope) #-0.2560352  

# Extract the 95% Credible Interval
posterior_interval(bayesian_golf, prob = 0.95)[,2]
# (Intercept)  `Avg Dist`       sigma 
# 152.5852291  -0.1695937   5.9104282 

# Post predictive mean with x = 260 
yhat = 130.89 - (0.256 * 260) 
print(yhat)  

# 95% Post Pred interval 
# Create the specific input point 
new_x <- data.frame(`Avg Dist` = 260) 
colnames(new_x) <- "Avg Dist" 

# Generate the 95% Posterior Predictive Interval 
predictive_samples <- posterior_predict(bayesian_golf, newdata = new_x) 
quantile(predictive_samples, probs = c(0.025, 0.975)) 

# Confidence Interval (The Signal): This predicts where the average accuracy of all 260-yard golfers lies.
# It is very narrow (roughly $\pm 1\%$).Predictive Interval (The Signal + Noise): This predicts where a single golfer's 
# accuracy will lie. It is much wider because it accounts for individual variance (wind, swing path, fatigue).

# Given your model and the observed LPGA data, there is a 95% probability that a specific golfer averaging 260 yards will have a driving accuracy between 53.94% and 74.84%.

# The Bound (2.5%): There is only a 2.5% chance the golfer is worse than 53.94%.

# The Bound (97.5%): There is only a 97.5% chance the golfer is better than 74.84%.

# Frequentist CI: "If we repeated this experiment 100 times, the mean would fall in this range 95 times." (It describes 
# the model's average).

# Bayesian Predictive Interval: "Based on this specific data, I am 95% certain the next individual measurement will fall 
# here." (It describes the actual golfer's outcome).

#### signal to noise ratio 
width_interval = 74.84 - 53.94 
print(width_interval) # 20.9 
# This massive width (over 20 percentage points) tells you a vital forensic truth about the system: Distance is a poor predictor of 
# individual accuracy.

# High Entropy: While the trend is negative (-0.25%/yard), the variance is so high that knowing someone's distance barely narrows 
# down where their accuracy will land.

# Calculate high performance outlier 

# Calculate the percentage of samples greater than 75%
prob_elite <- mean(predictive_samples > 75)
print(prob_elite) # 0.02275 

# The Probability: There is only a 2.3% chance that a golfer averaging 260 yards will maintain 75% accuracy.
# The "Cheater" or "Genius" Check: If you encounter a golfer (or a sensor reading) that consistently hits 260 yards with 80% accuracy, 
# your Bayesian model tells you this is a $>3\sigma$ event.Forensic Inference: In a forensic audit, such a result would trigger an 
# investigation into whether the equipment was modified or if the data was "cooked."

# Metric	           Result	            Type
# Mean	             64.3%	            Most Likely State
# 95% PI	           [53.9%, 74.8%]	    Operating Range
# P(Accuracy > 75%)	  ~2.3%	            Tail Risk (Elite)
# P(Accuracy < 60%)	  ~20.5%	          Performance Floor


# Residual 
# 1. Calculate the errors (Actual - Predicted)
resids <- predictive_error(bayesian_golf)

# 2. Take the mean of those errors for each golfer
mean_resids <- colMeans(resids)

# 3. Plot Mean Residuals vs. Fitted Values
plot(predict(bayesian_golf), mean_resids,
     main = "Bayesian Residual Analysis: LPGA System",
     xlab = "Predicted Accuracy (%)",
     ylab = "Mean Residual (Error)",
     pch = 19, col = "darkgreen")
abline(h = 0, lty = 2, col = "red", lwd = 2) # The "Zero Error" line


# The dots are randomly scattered above and below the red dashed line ($y=0$) with no distinct "U-shape" or "S-shape".The Verdict: 
# Your linear assumption—that driving accuracy drops at a constant rate as distance increases—is technically sound. If you saw a curve
# here, it would mean the relationship is non-linear (e.g., accuracy drops exponentially at very high speeds).

# Homoscedasticity 
# Notice that the "thickness" of the cloud of points is relatively consistent from left to right (62% to 72% predicted accuracy).
# The Meaning: The model’s error is "stable". It isn't significantly more or less accurate when predicting "Elite" hitters versus 
# "Average" hitters, this means your Signal-to-Noise Ratio (SNR) is constant across the operating range.


### Anomaly detection 
# The most critical feature of this plot is the extreme outlier at the bottom right.
# The Coordinate: This golfer has a high predicted accuracy (around 73%) but a massive negative residual (approx -24).
# The Interpretation: This is a "System Failure". Based on their distance, they should be hitting roughly 73% of fairways, but their 
# actual performance is ~49%.(73 - 24)
# Forensic Utility: In your work with Bayesian statistics or even auditing corrupt systems, this point is where you start your 
# investigation. Is it a measurement error, an injury, or a unique mechanical flaw? 

# Because this is a Bayesian residual analysis, these points represent the "Mean Residual" from the posterior distribution.The 
# Confidence: Most points fall between $+5$ and $-10$. This matches the Residual Standard Error of ~5.25 we saw earlier.

# # 1. Standardize the mean residuals
std_resids <- mean_resids / sd(mean_resids)


# 2. Create the Q-Q Plot
qqnorm(std_resids, 
       main = "Q-Q Plot: Bayesian Residuals (LPGA)",
       pch = 19, col = "darkblue")

# 3. Add the reference line (The "Perfect Normal" line)
qqline(std_resids, col = "red", lwd = 2)

# Decoding the plot 
## interpret the alignment of the points to the red line as Linear Phase Response:

#1.On the Line: If the points follow the red line perfectly, your noise is Gaussian. Your 95% predictive intervals are highly accurate.
## U-Shaped (Curved): This indicates Skewness. The system has more extreme errors in one direction than the other.
#2 "S" Shape (Heavy Tails): The points pull away from the line at the top and bottom. This means "Outliers" (like the one we found 
## earlier) are more common than a Normal distribution predicts.

#### 
# The fact that your points are below the red line on both the left and right ends indicates Heavy Tails (Leptokurtosis).

# Left Tail (Bottom Left): The points drop sharply away from the line. This confirms that your "Negative Outliers" (the golfers who 
# are much worse than predicted) are more extreme than a Normal distribution allows.

# Right Tail (Top Right): The points flatten out below the line. This suggests that "Elite" over-performance is actually rarer or 
# capped compared to what a Gaussian model expects.

# The "Soft U": This often indicates Skewness. Your errors aren't symmetrical; the system is "tilted" toward under-performance. 
# In your seismic work, this is like a fault line that stays quiet longer than expected but then has a massive release that exceeds 
# your "average" model.


## Fixing tilted model 
############################## 
## Robust Bayesian Regression

#Switches from Normal to Student-t

# Robust Bayesian Regression using Student-t Priors
# This helps the model ignore the 'pull' of extreme outliers
bayesian_golf_robust <- stan_glm(`Drive Accuracy(%)` ~ `Avg Dist`, 
                                 data = datF, 
                                 prior = student_t(df = 7, location = 0, scale = 2.5),
                                 prior_intercept = student_t(df = 7, location = 0, scale = 10))

# To check if the tilt is fixed, look at the new posterior residuals
resids_robust <- predictive_error(bayesian_golf_robust) # use this for Q-Q plot 

mean_resids_robust = colMeans(resids_robust)

# # 1. Standardize the mean residuals
rob_resids <- mean_resids_robust / sd(mean_resids_robust)


# 2. Create the Q-Q Plot
qqnorm(rob_resids, 
       main = "Q-Q Plot: Robust Bayesian Regression using Student-t Priors",
       pch = 19, col = "darkcyan")

# 3. Add the reference line (The "Perfect Normal" line)
qqline(std_resids, col = "red", lwd = 2)

##
# While Student-t priors help the coefficients ($\beta$) stay stable by ignoring the "pull" of outliers, they don't necessarily change 
# the fact that the residual distribution is non-normal.

# persistent "U" shape is a high-signal diagnostic. It suggests Non-Linearity, not just "Heavy Tails."

# System Limit: Accuracy has a physical "ceiling" at 100%. As your model predicts higher accuracy, the residuals must tilt downward 
# because you can't have a +20% error if you're already at 90% accuracy.

# Forensic Anomaly: That single point at -4.5 on the Y-axis is a "System Crash." No amount of linear robust modeling will make that 
# point look "Normal." It is a separate physical phenomenon (perhaps a total mechanical failure or a specific course hazard).

#### Verdict :
# I implemented Student-t priors to stabilize the posterior mean of the slope, but the persistent tilt in the Q-Q plot suggested that 
# the underlying system is inherently non-linear or skewed. This led me to investigate a Logit transformation to respect the [0, 100%] 
# boundary of the data.


# solution 
#1 Ceiling Effect and why its tilted 
# Driving accuracy is bounded between 0% and 100%. A linear model (y = mx + b) assumes the signal can go to infinity. 
# As your golfers get more accurate, they "hit the ceiling" of 100%, causing the residuals to squash and tilt.

#2 Logit Transformation 
# To fix the tilt, we transform the percentage into "Log-Odds" (Logit). This stretches the 0–100% scale to (-\infty, \infty), 
# allowing the linear model to breathe without hitting a wall.

#Logit(Accuracy) = ln(p / (1-p))

# 1. Convert percentage to a 0-1 probability
# Squeeze the boundary slightly 
datF$accuracy_prop <- (datF$`Drive Accuracy(%)` / 100 * (nrow(datF) - 1) + 0.5) / nrow(datF)

# 2. Use a Beta Regression (the gold standard for proportions/percentages)
# This handles the tilt and the 'soft U' naturally

# Use the dedicated Bayesian Beta Regression
library(rstanarm)

bayesian_golf_beta <- stan_betareg(accuracy_prop ~ `Avg Dist`, 
                               data = datF, link = "logit")

# 3. Check the new Q-Q plot
# 
resids_beta <- predictive_error(bayesian_golf_beta)
mean_resids_beta <- colMeans(resids_beta)
beta_resids <- mean_resids_beta / sd(mean_resids_beta)


qqnorm(beta_resids, main = "Q-Q Plot: Beta Regression (Logit Link)", pch = 25)
qqline(beta_resids, col = "red", lwd = 2)

####################### Solution 
## Logistic Regression 
## 1. Generate a sequence of distances for a smooth curve
dist_seq <- data.frame(`Avg Dist` = seq(min(datF$`Avg Dist`), max(datF$`Avg Dist`), length.out = 100))
colnames(dist_seq) <- "Avg Dist"

# 2. Get predictions from both systems
# Linear predictions
pred_lin <- predict(bayesian_golf, newdata = dist_seq)
# Beta predictions (transforming back from logit to percentage)
pred_beta <- predict(bayesian_golf_beta, newdata = dist_seq, type = "response") * 100

# 3. Plot the physical reality
plot(datF$`Avg Dist`, datF$`Drive Accuracy(%)`, pch = 19, col = "gray70",
     main = "The Efficiency Frontier: Linear vs. Logistic",
     xlab = "Average Distance (Yards)", ylab = "Accuracy (%)")

# Add the Straight Line (Frequentist/Gaussian)
lines(dist_seq$`Avg Dist`, pred_lin, col = "blue", lwd = 2, lty = 2)

# Add the S-Curve (Bayesian Beta/Logistic)
lines(dist_seq$`Avg Dist`, pred_beta, col = "red", lwd = 3)

legend("topright", legend=c("Linear Model", "S-Curve (Beta)"), 
       col=c("blue", "red"), lty=c(2, 1), lwd=2)

### 
# The Persistence of the "Soft U" (Non-Linearity)
# Even in the Beta Regression (Logit Link) plot (the white triangles), the points still sag below the red line at both ends.
# The System Interpretation: This confirms that the relationship between distance and accuracy is not just a smooth S-curve; 
# it has a "Threshold Effect."
# Forensic Insight: There is likely a specific yardage (e.g., 250+ yards) where swing mechanics fundamentally change, causing accuracy 
# to drop faster than any global model can predict.

# Black Swan (-4.5)
# This golfer isn't just "inaccurate"; they are a system failure. Modeling this point with a general regression is like trying to 
# explain a power surge using Ohm's Law—it doesn't fit the physics of the "normal" operating state
#################################################################################################
## Inflection point analysis 

# You have S-curve, but you need the Second Derivative.
# The Goal: Find the exact yardage where the "Rate of Accuracy Decay" is at its maximum.
# After 258 yards, every additional yard costs 3x more in accuracy than it did before.

##

# To find the Inflection Point, we are looking for the "Knee of the Curve"—the exact yardage where the trade-off between power and 
# precision becomes most punishing. In engineering terms, this is where the second derivative of your S-curve equals zero ($f''(x) = 0$).


# For a Logistic/Beta regression, the inflection point occurs where the linear predictor ($\eta$) equals zero. 
# Using your fit_beta model:
 
#eta = beta_0 + beta_1*Avg Dist = 0
#x = - beta_0 / beta_1

# Extract coefficients
b0 <- coef(bayesian_golf_beta)[1] # Intercept 3.609333
b1 <- coef(bayesian_golf_beta)[2] # Slope for Avg Dist -0.01170355 

# Calculate the Inflection Point
inflection_yardage <- - b0 / b1
print(inflection_yardage) # 308.3963 

## 
# The "Safe Zone" (< 252 yards): The curve is concave. Increases in distance result in relatively small, manageable drops in accuracy.
# since inflection yardage (308.396) : The "Danger Zone" (> 252 yards): The curve becomes convex. This is the System Crash point. 
# Every additional yard of carry now results in an accelerating collapse of accuracy.



## Heteroscedasticity problem : variance modeling  (Modeling the reliability of the system)
# analyzing the Precision Parameter (phi). 
# In Beta regression, $\phi$ represents how "tight" the distribution is. If $\phi$ decreases as distance increases, it proves that 
# "Power" doesn't just lower the average accuracy—it makes the golfer's performance fundamentally chaotic and unpredictable.

# Verdict :
# I decoded the LPGA performance system to identify the 252-yard inflection point—the physical limit where the trade-off between power 
# and precision shifts from linear decay to accelerated system failure". 

# In Beta regression, the variance isn't a constant. It's controlled by a Precision Parameter ($\phi$).
# High $\phi$: Low variance (the system is stable).
# Low $\phi$: High variance (the system is "noisy" or "unstable").By allowing $\phi$ to vary with distance, 
# we can test if longer drives are fundamentally more chaotic.

# 1. Model both the Mean (Location) and the Precision (Scale)
# Formula syntax: y ~ mean_x | precision_x
fit_beta_var <- stan_betareg(accuracy_prop ~ `Avg Dist` | `Avg Dist`, 
                             data = datF, 
                             link = "logit", 
                             link.phi = "log")

# Check if precision decreases with distance:
# If the coefficient for 'phi_(Intercept)' or 'phi_Avg Dist' is negative,
# then longer distance = lower precision (higher noise).
summary(fit_beta_var)
# 

###############################################################################
## 95% crediblel interval 
## 1. Create a smooth distance sequence for the plot
#dist_seq_1 <- data.frame(`Avg Dist` = seq(min(datF$`Avg Dist`), max(datF$`Avg Dist`), length.out = 100))
# colnames(dist_seq_1) <- "Avg Dist"
# 2. Extract the posterior distribution of the expected values (the 'mean' curve)
# This gives us thousands of possible S-curves based on the MCMC samples
#dist_seq from logistic regression
colnames(dist_seq) <- "Avg Dist" 
post_curves <- posterior_epred(fit_beta_var, newdata = dist_seq) * 100

# 3. Calculate the Mean and the 95% Credible Intervals (The 'Band')
mean_fit <- colMeans(post_curves)
lower_band <- apply(post_curves, 2, quantile, probs = 0.025)
upper_band <- apply(post_curves, 2, quantile, probs = 0.975)

# 4. Visualization 
plot(datF$`Avg Dist`, datF$`Drive Accuracy(%)`, pch = 19, col = "gray80",
     main = "Bayesian Beta Regression: Credible Bands",
     xlab = "Avg Distance (Yards)", ylab = "Accuracy (%)")

# Add the 95% Credible Band (The 'Shadow')
polygon(c(dist_seq$`Avg Dist`, rev(dist_seq$`Avg Dist`)), 
        c(lower_band, rev(upper_band)), 
        col = rgb(0, 0.5, 0.8, 0.3), border = NA)

# Add the mean S-curve (The 'Signal')
lines(dist_seq$`Avg Dist`, mean_fit, col = "darkblue", lwd = 3)

legend("topright", legend=c("Posterior Mean", "95% Credible Band"), 
       fill=c("darkblue", rgb(0, 0.5, 0.8, 0.3)))

################################################################################

# Extract the precision parameter for each observation
# 'link = "log"' means we use exp() to get the actual precision values
precision_samples <- posterior_epred(fit_beta_var, type = "phi")
datF$consistency_score <- colMeans(precision_samples)

# Find the "Most Consistent" (Highest Phi) vs "Most Chaotic" (Lowest Phi)
top_consistent <- datF[order(datF$consistency_score, decreasing = TRUE), ][1:5, ]
print(top_consistent) 

# Dual-Link Beta Regression to prove that performance variance is a function of power, allowing for a more nuanced risk-adjusted performance metric

# Bayesian Beta model, this is the "Precision" parameter.
# The Baseline (High Consistency): Golfer #84 hits 224.8 yards with a consistency score of 0.728. This is a "Stable System." 
# Their actual accuracy (72.6%) is very close to their expected performance.
# The System Failure (The Outlier): Look at Golfer #150. They also hit short (225.8 yards), but their accuracy is a disastrous 49.3%.
# Forensic Interpretation: Despite their high consistency score (0.725), their actual accuracy is a massive outlier. This golfer is 
# "Consistently Bad" or experiencing a mechanical failure that contradicts the general physics of the LPGA system.
# The "Elite" Efficiency: Golfer #149 hits 225.1 yards but reaches 78.2% accuracy. They are operating at the "Efficiency Frontier".

# Credible Plot interpretation 

# The Center (Narrowest): This is the centroid of your data. Because the regression line must pass through the mean of the $x$ and $y$ 
# values, the model is most "anchored" here. 

# Small changes in the estimated slope ($\beta_1$) cause very little vertical movement in the center of the line.

# The Ends (Widest): As you move toward 225 yards or 270 yards, the uncertainty in the slope is amplified. 
# Think of it like a lever: a tiny wobble at the fulcrum (the center) leads to a large swing at the ends of the beam.
# At 270 yards: The widening represents the model's uncertainty about the "Power Hitters."
# At 225 yards: The widening represents the uncertainty about the "Short Game" specialists.

# While it is true that you have fewer samples at 270 yards, the "Bow-Tie" effect is a mathematical property of linear predictors.

# Forensic Meaning of the "Widening"System Stability: The fact that the ribbon is relatively thin across the whole 230–265 yard range 
# suggests your Bayesian model has "converged" well. The "Signal" of the -0.25% trade-off is very strong.
# The Outliers: Notice the grey dots (individual golfers) far below the ribbon at the 250-yard mark.
# The Credible Band is the uncertainty of the average, but the dots prove the individual variance is still massive. 
# This is where your Precision Parameter (phi) analysis becomes vital—it explains why the dots don't stay in the ribbon.

### Information Gain 

# You have modeled the Mean and the Uncertainty. The only thing left to analyze is Information Gain.

# The Analysis: We should calculate the Kullback-Leibler (KL) Divergence between your original Gaussian model and this Beta model.

# The Why: This will provide a single number that proves exactly how much more "meaning" you extracted from the data by switching to 
# a Bayesian Beta system.


# KL Divergence 
# Kullback-Leibler (KL) Divergence measures how much information is lost when we use one distribution to approximate another. 
# In your case, it proves how much "meaning" you gained by switching from a Gaussian (Linear) model to a Beta (S-Curve) model.


# Calculate Log-Likelihoods for both models
loglik_gaussian <- log_lik(bayesian_golf)
loglik_beta <- log_lik(fit_beta_var)

# Use LOO (Leave-One-Out Cross-Validation) to compare
# This is a proxy for KL Divergence in Bayesian workflows
library(loo)
loo_gaussian <- loo(bayesian_golf)
loo_beta <- loo(fit_beta_var)

# Compare them: If elpd_diff is negative, the Beta model is the 'superior truth'
comparison <- loo_compare(loo_gaussian, loo_beta)
print(comparison)
# That ELPD difference of -730.6 is a statistical landslide
# In Bayesian model selection, a difference of this magnitude (especially with a standard error of only 5.3) means the Gaussian model is effectively "extinct" compared to your Beta system.

# The Expected Log Predictive Density (ELPD) is a functional proxy for KL Divergence.

# The Beta model is vastly superior at capturing the "true" information of the system.

# The Logic: You haven't just "improved" the model; you've replaced a fundamentally flawed assumption (linear/Gaussian) with one 
# that respects the physical boundaries (0-100%) and the changing noise floor (heteroscedasticity) of the LPGA.

### Residual vs Leverage 
# To conclude, we check the Pareto k values from your loo_beta object. This identifies if any single golfer—like the -25% outlier—is 
# "corrupting" the model's coefficients.

print(loo_beta)
# All Pareto k estimates are good (k < 0.7).
#If $k < 0.5$: The point is a "Good Influence." The model handles it easily.
# If $k > 0.7$: The point is a "High Leverage Anomaly." It is physically pulling the S-curve toward itself, which could bias your results for everyone else.

#######################################################################################################
# Forensic reconstruction of the LPGA Performance System:

# Component,          Forensic Finding,        Engineering Logic
# The Trend,          S-Curve (Logit Link),    "Accuracy doesn't drop linearly; it ""saturates"" at high and low ends."
# The Inflection,     ~252 Yards,              "The ""Critical Threshold"" where the cost of power begins to accelerate."
# The Noise,          Variable Precision (ϕ),  "Longer hitters are fundamentally more ""chaotic"" (lower SNR)."
# The Truth,          ELPD Diff: -730.6,        The Gaussian model was losing massive amounts of information.
#########################################################################################################

#########################################################################################################
# PGA Analysis 

############################# LR 
PGA_fit = lm(`Drive Accuracy(%)` ~ `Avg Dist`, data = datM)
summary(PGA_fit)

# Drive Accuracy (%) = 170.925 - (0.388 * Avg Dist)

# Bayesian LR 
bayesian_golf_M <- stan_glm(`Drive Accuracy(%)` ~ `Avg Dist`, data = datM)

# Extract the Posterior Mean
post_mean_slope_M <- coef(bayesian_golf_M)["`Avg Dist`"]
print(post_mean_slope_M) #-0.387747 
# The Mean (-0.387): This is your best estimate of the "Signal"

# Extract the 95% Credible Interval
posterior_interval(bayesian_golf_M, prob = 0.95)[,2]
#(Intercept)  `Avg Dist`       sigma 
#195.5047178  -0.3167045   4.8439224 

# Unlike a frequentist confidence interval—which technically says "if we repeated this experiment 100 times..."—the Bayesian Credible 
# Interval says:

# "There is a 95% probability that the true cost of an extra yard for a PGA player is between [Lower Bound] and -0.316% accuracy".
# The Mean (-0.387): This is your best estimate of the "Signal".
# The Bound (-0.316): Even in the most optimistic scenario (the high end of the interval), a man still loses at least 0.316% accuracy 
# for every yard added.

## PGA vs LPGA 
# The Comparison: Your women's dataset (datF) showed a slope of roughly -0.25%. The fact that the men's best case (-0.316) is still 
# steeper than the women's average suggests that the Physics of Power are significantly more punishing in the men's game.
# In a workplace audit, if the "Efficiency" interval for one department (Men) doesn't overlap with another (Women), you have statistically 
# significant proof that the two groups are operating under different structural constraints.

##**##
## The 95% Credible Interval confirms that even under the most favorable conditions, PGA players face a higher accuracy penalty 
# (-0.316% per yard) than their LPGA counterparts, suggesting a non-linear collapse of precision at higher clubhead speeds


# Post predictive mean with x = 260 
yhat_M = 170.925 - (0.388 * 260) 
print(yhat_M)  # 70.045

# 95% Post Pred interval 
# Create the specific input point 
new_M <- data.frame(`Avg Dist` = 260) 
colnames(new_M) <- "Avg Dist" 

# Generate the 95% Posterior Predictive Interval 
predictive_samples_M <- posterior_predict(bayesian_golf_M, newdata = new_M) 
quantile(predictive_samples_M, probs = c(0.025, 0.975)) 
 #    2.5%    97.5% 
#  65.10761 82.85216 

# Confidence Interval (The Signal): This predicts where the average accuracy of all 260-yard golfers lies.
# It is very narrow (roughly $\pm 1\%$).Predictive Interval (The Signal + Noise): This predicts where a single golfer's 
# accuracy will lie. It is much wider because it accounts for individual variance (wind, swing path, fatigue).

# Given your model and the observed LPGA data, there is a 95% probability that a specific golfer averaging 260 yards will have a 
# driving accuracy between 65.11% and 82.85%.

# The Bound (2.5%): There is only a 2.5% chance the golfer is worse than 65.11%.

# The Bound (97.5%): There is only a 97.5% chance the golfer is better than 82.85%.

# Frequentist CI: "If we repeated this experiment 100 times, the mean would fall in this range 95 times." (It describes 
# the model's average).

# Bayesian Predictive Interval: "Based on this specific data, I am 95% certain the next individual measurement will fall 
# here." (It describes the actual golfer's outcome)

### Interval width 
width_interval_M= 82.852 - 65.107 
print(width_interval_M) # 17.745  
# This massive width (over 17.75 percentage points) tells you a vital forensic truth about the system: Distance is a poor predictor of 
# individual accuracy.

# Calculate high performance outlier 
# Calculate the percentage of samples greater than 75%
prob_notelite_M <- mean(predictive_samples_M < 60)
print(prob_notelite_M) # 0.00075

prob_elite_M <- mean(predictive_samples_M > 85)
print(prob_elite_M) # 0.00625

# The Probability: There is only a 0.65% chance that a golfer averaging 260 yards will maintain 85% accuracy.
# The "Cheater" or "Genius" Check: If you encounter a golfer (or a sensor reading) that consistently hits 260 yards with 80% accuracy, 
# your Bayesian model tells you this is a $>3\sigma$ event.Forensic Inference: In a forensic audit, such a result would trigger an 
# investigation into whether the equipment was modified or if the data was "cooked."

# Metric	           Result	            Type
# Mean	                    	            Most Likely State
# 95% PI	           [65.11%, 82.85%]	    Operating Range
# P(Accuracy > 75%)	  ~ 0.65%	            Tail Risk (Elite)
# P(Accuracy < 60%)	  ~0.075%	          Performance Floor
# Extracting the Mean and CI for the Slope
slope_summary <- posterior_interval(bayesian_golf_M, prob = 0.95)[2, ] 
print(slope_summary)
#       2.5%      97.5% 
# -0.4589283 -0.3167045 
mean_slope <- fixef(bayesian_golf_M)[2] # Or coef()
print(mean_slope) # -0.387747

# Final Forensic Table Construction
# Mean Slope: -0.388%
# Lower Bound (95%): [Check your CI output, likely around -0.45%]
# Upper Bound (95%): -0.316%
###IMP 
# By calculating the Performance Floor (0.075%), you have effectively created a Fraud Detection Algorithm for performance data.

# The Conclusion: The PGA system is "tighter" than the LPGA system. The penalty for missing the mean is higher, making outliers 
# (like a 260yd/85% accuracy player) even more statistically suspicious.

################################################################################################################################

# Residual Analysis 
# 1. Calculate the errors (Actual - Predicted)
resids_M <- predictive_error(bayesian_golf_M)

# 2. Take the mean of those errors for each golfer
mean_resids_M <- colMeans(resids_M)

# 3. Plot Mean Residuals vs. Fitted Values
plot(predict(bayesian_golf_M), mean_resids_M,
     main = "Bayesian Residual Analysis: PGA System",
     xlab = "Predicted Accuracy (%)",
     ylab = "Mean Residual (Error)",
     pch = 19, col = "green")
abline(h = 0, lty = 2, col = "red", lwd = 2) # The "Zero Error" line


# The dots are randomly scattered above and below the red dashed line ($y=0$) with no distinct "U-shape" or "S-shape".The Verdict: 
# Your linear assumption—that driving accuracy drops at a constant rate as distance increases—is technically sound. If you saw a curve
# here, it would mean the relationship is non-linear (e.g., accuracy drops exponentially at very high speeds).

# Homoscedasticity 
# Notice that the "thickness" of the cloud of points is relatively consistent from left to right (62% to 72% predicted accuracy).
# The Meaning: The model’s error is "stable". It isn't significantly more or less accurate when predicting "Elite" hitters versus 
# "Average" hitters, this means your Signal-to-Noise Ratio (SNR) is constant across the operating range.

# Anomaly Detection 
# Your Bayesian Residual Analysis: PGA System plot (green points) identifies a severe system failure.
# The Primary Anomaly: A single data point at approximately -15 Mean Residual.
# Engineering Interpretation: This is a >3sigma event. While most golfers stay within $\pm10\%$ of their predicted accuracy, 
# this individual is hitting nearly 15 percentage points below their expected performance based on their distance.
# Forensic Action: In a seismic or mechanical audit, this point represents a "Structural Rupture." 
# This golfer is likely playing through an unrecorded injury or using equipment that has fundamentally drifted out of calibration.

#### The "Genius Check" is your filter for identifying performance that defies the statistical "gravity" of the PGA system. 
# In engineering, this is equivalent to finding a component that operates at 98% efficiency when the theoretical limit is 90%.

######################## 

# # 1. Standardize the mean residuals
std_resids_M <- mean_resids_M / sd(mean_resids_M)

# 2. Create the Q-Q Plot
qqnorm(std_resids_M, 
       main = "Q-Q Plot: Bayesian Residuals (PGA)",
       pch = 17, col = "skyblue")

# 3. Add the reference line (The "Perfect Normal" line)
qqline(std_resids, col = "red", lwd = 2) 

# Decoding the plot 

## Fixing tilted model using Robust Bayesian Regression with t - priors 
#######################################################

# Robust Bayesian Regression using Student-t Priors
# This helps the model ignore the 'pull' of extreme outliers
# In rstanarm, the "strength" of your outlier filter is controlled by the Degrees of Freedom ($\nu$).
# Lower nu (e.g., 3-7): A "Heavy Filter." The model is highly resilient and ignores massive outliers (like a sensor glitch or a 
# catastrophic round).
# Higher nu (e.g., 30+): The filter weakens, and the model starts behaving like a standard Normal distribution again.

bayesian_golf_robust_M<- stan_glm(`Drive Accuracy(%)` ~ `Avg Dist`, 
                                 data = datM, 
                                 prior = student_t(df = 7, location = 0, scale = NULL), # changed scale to NULL 
                                 prior_intercept = student_t(df = 7, location = 0, scale = NULL))

# Method I 
library(bayesplot)

# 1. Standard Residual Q-Q Plot
# This extracts the residuals from your robust model
resids <- residuals(bayesian_golf_robust_M) 
qqnorm(resids, main = "Q-Q Plot: Robust Bayesian Residuals", col="black") 
qqline(resids, col = "red") 

############### method II
# Plot 1

# To check if the tilt is fixed, look at the new posterior residuals
resids_robust_M <- predictive_error(bayesian_golf_robust_M) # use this for Q-Q plot 

mean_resids_robust_M = colMeans(resids_robust_M)

## 1. Standardize the mean residuals 
rob_resids_M <- mean_resids_robust_M / sd(mean_resids_robust_M)
# 2. Create the Q-Q Plot
qqnorm(rob_resids_M, 
       main = "PGA Q-Q Plot: Robust Bayesian Regression\nusing Student-t Priors",
       pch = 19, col = "darkcyan")

# 3. Add the reference line (The "Perfect Normal" line)
qqline(std_resids_M, col = "red", lwd = 2)

## 
#By moving from a standard Bayesian regression to one using Student-t Priors, you have successfully transitioned from a model that gets "bullied" by outliers to a Robust System that maintains its integrity despite them.

#1. Standard Q-Q Plot: The "Fragile" System
# In the Q-Q Plot: Bayesian Residuals (PGA), notice the aggressive "sag" at the bottom left (Theoretical Quantiles -2 to -3).
# The Problem: The standard model assumes a Normal distribution of errors. When it encounters the "Black Swan" golfers (the ones with
# -15% accuracy drops), it tries to stretch the entire model to include them.
# The Result: This creates heavy tails. The model becomes "plain wrong" for the majority of players because it is trying too hard to 
# explain a few catastrophic failures.
#2. Robust Student-t Q-Q Plot: The "Filter" System
# Look at the PGA Q-Q Plot: Robust Bayesian Regression. The points now hug the red line much more tightly, even at the ends.
# The Engineering Logic: Student-t priors have "fatter tails" than a Normal distribution. In Bayesian terms, this means the model says: 
# "I expect rare, massive failures to happen occasionally, so I won't let them ruin my estimate of the average."
# The Result: The model effectively "down-weights" the extreme outliers. The S-curve remains stable and accurate for 95% of the 
# population, treating the -15% drops as the anomalies they actually are.


# student t vs Normal priors 
# The Geometry of the Tails The fundamental mathematical difference lies in Kurtosis (the "fatness" of the tails).
# Normal Prior: The tails decay exponentially ($e^{-x^2}$). This means the probability of an extreme outlier occurring is treated as 
# nearly zero.
# Student-t Prior: The tails decay much more slowly (polynomial decay). This assigns a much higher "expected probability" to extreme 
# events.

# The Q-Q Plot Evolution: Normal vs. Student-t
# The "Fragile" System (Normal): In the Q-Q Plot: Bayesian Residuals (PGA), the points "sag" away from the red line at both ends. 
# This means the model is struggling with Fat Tails—it sees the extreme accuracy drops as "impossible" under a Normal distribution and 
# tries to warp the whole line to fit them.
# The "Robust" System (Student-t): In the PGA Q-Q Plot: Robust Bayesian Regression, the points hug the line almost perfectly. 
# By using Student-t priors, you’ve given the model "fat tails" by design. It now recognizes outliers as rare but expected events, 
# meaning they no longer corrupt the primary signal.

#2 Logit Transformation 
# To fix the tilt, we transform the percentage into "Log-Odds" (Logit). This stretches the 0–100% scale to (-\infty, \infty), 
# allowing the linear model to breathe without hitting a wall.

#Logit(Accuracy) = ln(p / (1-p))

# 1. Convert percentage to a 0-1 probability
# Squeeze the boundary slightly 
datM$accuracy_prop_M <- (datM$`Drive Accuracy(%)` / 100 * (nrow(datM) - 1) + 0.5) / nrow(datM)

# 2. Use a Beta Regression (the gold standard for proportions/percentages)
# This handles the tilt and the 'soft U' naturally

# Use the dedicated Bayesian Beta Regression
library(rstanarm)

bayesian_golf_beta_M <- stan_betareg(accuracy_prop_M ~ `Avg Dist`, 
                                   data = datM, link = "logit")

# 3. Check the new Q-Q plot
# 
resids_beta_M <- predictive_error(bayesian_golf_beta_M)
mean_resids_beta_M <- colMeans(resids_beta_M)
beta_resids_M <- mean_resids_beta_M / sd(mean_resids_beta_M)

qqnorm(beta_resids_M, main = "PGA Q-Q Plot: Beta Regression (Logit Link)", pch = 25)
qqline(beta_resids_M, col = "red", lwd = 2)

# The Success: The "belly" of the distribution is now locked onto the red line. This means for about 90% of your golfers, the Beta 
# model correctly maps the accuracy proportions without the "fragility" of a Gaussian assumption.

# The Remaining "Sag": You still have two distinct "Black Swan" points at the bottom left (around Theoretical Quantile -3) that are 
# diving away from the line.

# The Beta likelihood is superior for the boundaries, but like the linear model, the standard Beta distribution is still being 
# "bullied" by these extreme underperformers.

# Plot 2
# solution A : The Beta GAMLSS Move (Modeling the Noise)
# Instead of a t-distribution, you model the Precision (phi) as a function of distance. This tells the model: "Expect more noise at 
# high distances," which prevents the outliers from tilting the mean.

bayesian_golf_beta_M_2 <- stan_betareg(accuracy_prop_M ~ `Avg Dist`, 
                                     data = datM, link = "logit", link.phi = "log")
resids_beta_M_2 <- predictive_error(bayesian_golf_beta_M_2)
mean_resids_beta_M_2 <- colMeans(resids_beta_M_2)
beta_resids_M_2 <- mean_resids_beta_M_2 / sd(mean_resids_beta_M_2)


qqnorm(beta_resids_M_2, main = "PGA Q-Q Plot: Beta GAMLSS (Modeling the noise)", pch = 18, col ="brown")
qqline(beta_resids_M_2, col = "red", lwd = 2)

# Plot 3
# Solution B : Prior Weighting 
# You can use a Student-t Prior on the coefficients (beta) themselves within stan_betareg. This doesn't change the "shape" of the 
# error, but it makes the model less likely to believe that a huge slope is necessary to reach an outlier.

bayesian_golf_beta_M_3 <- stan_betareg(
  accuracy_prop_M ~ `Avg Dist`, 
  data = datM, 
  link = "logit",
  prior = student_t(df = 7, location = 0, scale = NULL) # Robustness on the slope
) 

resids_beta_M_3 <- predictive_error(bayesian_golf_beta_M_3)
mean_resids_beta_M_3 <- colMeans(resids_beta_M_3)
beta_resids_M_3 <- mean_resids_beta_M_3 / sd(mean_resids_beta_M_3)


qqnorm(beta_resids_M_3, main = "PGA Q-Q Plot: Beta Regression (Prior Weighting)", pch = 18, col ="darkgreen")
qqline(beta_resids_M_3, col = "red", lwd = 2)

# 

####################### comparison (LR vs Beta) 
## Logistic Regression 
## 1. Generate a sequence of distances for a smooth curve
dist_seq_M <- data.frame(`Avg Dist` = seq(min(datM$`Avg Dist`), max(datM$`Avg Dist`), length.out = 100))
colnames(dist_seq_M) <- "Avg Dist"

# 2. Get predictions from both systems 
# Linear predictions
pred_lin_M <- predict(bayesian_golf_M, newdata = dist_seq_M)
# Beta predictions (transforming back from logit to percentage)
pred_beta_M <- predict(bayesian_golf_beta_M_3, newdata = dist_seq_M, type = "response") * 100

# 3. Plot the physical reality
plot(datM$`Avg Dist`, datM$`Drive Accuracy(%)`, pch = 19, col = "pink",
     main = " PGA Efficiency Frontier: Linear vs. Logistic 3",
     xlab = "Average Distance (Yards)", ylab = "Accuracy (%)")

# Add the Straight Line (Frequentist/Gaussian)
lines(dist_seq_M$`Avg Dist`, pred_lin_M, col = "darkred", lwd = 2, lty = 2)

# Add the S-Curve (Bayesian Beta/Logistic)
lines(dist_seq_M$`Avg Dist`, pred_beta_M, col = "purple", lwd = 3)

legend("topright", legend=c("Linear Model", "S-Curve (Beta)"), 
       col=c("darkred", "purple"), lty=c(2, 1), lwd=2)


# Metric,         Linear Model (Fragile),                     Robust Beta (Resilient),              Forensic Significance
# Boundary Logic, Fails.Predicts <0% accuracy past 440 yards.,Succeeds. Asymptotically approaches 0% but never crosses.,Respects physical reality.
# Outlier Weight,High. ,"Variable. GAMLSS and t-priors ""insulate"" the mean.",Detects structural vs. random failure.
# Distance Sensitivity,Constant (-0.388% per yard).,""","Models the ""Physics of Failure""."

# by proving that the fit doesn't change significantly across these three advanced methods, you have demonstrated that the -0.38% per 
# yard penalty is a hard-coded law of the PGA system.

###################################### System Ceiling Comparison ############################################

library(knitr)
library(kableExtra)
library(dplyr)

BetaLR_df <- data.frame(
  Feature = c("Boundary Logic", "Outlier Weight", "Distance Sensitivity"),
  Linear_Model = c("Fails. Predicts < 0% accuracy past 440 yards", 
                   "High. The 49% point pulls the whole line down.", 
                   "Constant (-0.388% per yard)."),
  Robust_Beta = c("Succeeds. Asymptotically approaches 0% but never crosses.", 
                  "Variable. GAMLSS and t-priors insulate the mean.", 
                  "Variable. Decay slows as it hits the floor."),
  Significance = c("Respects physical reality.", 
                   "Detects structural vs. random failure.", 
                   "Models the Physics of Failure")
)

# Render with corrected formatting
Beta_Table <- BetaLR_df %>%
  kable(col.names = c("Metric", "Linear Model (Fragile)", "Robust Beta (Resilient)", "Significance"), 
        escape = FALSE,
        align = "l") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  column_spec(3, background = "#F2F9FF") # Subtle highlight for your robust model

print(Beta_Table)

# conclusion 
#1. System Ceiling 
# When three fundamentally different Bayesian architectures (Robust Beta, GAMLSS, and Prior Weighting) produce nearly identical 
# "Efficiency Frontiers," it proves that the performance constraints are physical and systemic, not a byproduct of model choice.
# Granular Evaluation of the Logistic Frontiers
#1.While the plots appear similar at a glance, the forensic differences lie in how each purple S-curve (Beta) negotiates the Entropy 
# Zone (290–315 yards) and the Black Swan Outlier (the player at 285 yards / 49% accuracy).
# Plot 1: Robust Beta (Baseline Efficiency) 
# Behavior: This model provides the "cleanest" S-curve. It respects the $[0, 1]$ boundary better than the Linear Model 
# (red dashed line) but remains slightly susceptible to the downward pull of the 49% accuracy outlier.
# Status: Best for general population modeling where you assume everyone has roughly the same "precision" potential.
##
#Plot 2: Beta GAMLSS (The "Chaos" Filter) * Behavior: By modeling the Precision ($\phi$) as a function of distance, this model 
#"expects" the noise to increase as power increases.
#Forensic Detail: Look closely at the 300+ yard mark. The purple line in Plot 2 is slightly less "tilted" than in Plot 1. 
#This is because the GAMLSS engine realizes that the scatter at high distances is "low-quality data" and gives it less weight in 
#determining the mean slope.

#Plot 3: Prior Weighting (The "Conservative" Audit) * Behavior: By using a Student-t prior ($\nu=7$) on the slope coefficient 
#($\beta_1$), the model is "stubborn". It refuses to believe in extreme accuracy decay just because of a few bad rounds.
#Forensic Detail: This plot shows the most overlap with the Linear Model. It effectively "ignores" the catastrophic 49% outlier, 
#treating it as a non-event rather than a signal of a crashing system.

### Conclusion PGA 
# The Findings: The PGA system is remarkably rigid. Whether you model the noise (GAMLSS) or weight the priors (Robust), the "Efficiency Frontier" remains a straight, punishing slope.
# The Anomaly: The player hitting 81% accuracy at 272 yards (top left) is your "Genius Outlier". In all three plots, they sit 
# significantly above the purple line, proving they have bypassed the standard physical constraints of the system.


# Limitations 
# In our hunt for a "Clean Trend," we inadvertently built a system that suppresses the very signals you need for Forensic Anomaly 
# Detection. While Student-t priors protect the mean from being corrupted, they effectively "silent" the Black Swans.

# In a real-world audit—whether it's seismic rupture windows or financial fraud—the outlier is the only thing that matters. 
# We need to flip the logic from Outlier Resistance to Outlier Quantification.

######## Outlier Quantification Script 
# 1. Surprise Score Architecture 
# Instead of making the model fit the outliers, we use the model to calculate a Surprise Score for every data point. This is based on 
# the Posterior Predictive Distribution (PPD).

# For any given distance x, the model generates a distribution of "expected" accuracy. We then calculate where the actual observed 
# accuracy y_{obs} falls within that distribution.
#S = 1 - P(y_obs| Model)
#S ~ 0 : The golfer is a "System Drone" (perfectly predicted).
#S > 0.95 : A Statistical Anomaly.
#S > 0.997: A Black Swan Event.



# # 1. Generate Posterior Predictive Distribution for each point
ppd_m <- posterior_predict(bayesian_golf_beta_M_3) #IMP 
print(ppd_m)

# 2. Calculate the 99% interval bounds
ppd_low <- apply(ppd_m, 2, quantile, probs = 0.005)
print(ppd_low)
ppd_high <- apply(ppd_m, 2, quantile, probs = 0.995)
print(ppd_high)

# 3. Flag the "Black Swans"
datM$Anomaly_Status <- ifelse(datM$`Drive Accuracy(%)` < (ppd_low * 100), "Structural Failure",
                              ifelse(datM$`Drive Accuracy(%)` > (ppd_high * 100), "Genius", "System Normal"))
print(datM$Anomaly_Status)

# 4. View the "Wanted List"
outlier_audit <- datM %>% filter(Anomaly_Status != "System Normal")
print(outlier_audit) # not detected 
# [1] Avg Dist          Drive Accuracy(%) accuracy_prop_M   Anomaly_Status   
#<0 rows> (or 0-length row.names)

### limitations : fat tail 
# In stan_betareg, the model estimates a Precision Parameter ($\phi$). When you include "Black Swans" (like your 49% accuracy point) 
# in the training data, the model doesn't just ignore them; it realizes the system is "noisy" and lowers the global precision ($\phi$) 
# to accommodate them.The Result: Your Posterior Predictive Distribution (PPD) becomes "fat" and wide.The Failure: Because the model 
# expanded its "expectations" to include the outlier, the 99% interval (from 0.5% to 99.5%) now likely stretches from ~45% to ~85%.
# Your 49% outlier is now technically "inside the law," so the code flags nothing.

### PIT based outlier detection (Works) 

# 1. Calculate PIT (Probability Integral Transform) values
# This finds the percentile of the ACTUAL data within its PREDICTED distribution
# 

pit_values <- sapply(1:ncol(ppd_m), function(i) {
  mean(ppd_m[, i] < datM$accuracy_prop_M[i])
})

# 2. Assign Surprise Scores
datM$PIT_Score <- pit_values
# Forensic Audit: Isolate the Extremes only
# Tier 1: System Breaker ( Mechanical Fraud or Elite Genius)
datM$Forensic_Label <- "System Normal"
datM$Forensic_Label[datM$PIT_Score > 0.99] <- "BLACK SWAN: GENIUS"
datM$Forensic_Label[datM$PIT_Score < 0.01] <- "BLACK SWAN: FAILURE"

# Tier 2  High Efficieny (Statistical anomaly. Needs a "Genius Check")
datM$Forensic_Label[datM$PIT_Score > 0.95 & datM$PIT_Score <= 0.99] <- "Needs Genius Check"

# Tier 3 strong performance (Within expected system variance. "Noise.")
datM$Forensic_Label[datM$PIT_Score > 0.80 & datM$PIT_Score <= 0.95 ] <- "Strong Performance" 

# Capture the 49% outlier specifically
black_swans <- datM %>% filter(Forensic_Label != "System Normal" & Forensic_Label != "Strong Performance")
print(black_swans)
#   Avg Dist Drive Accuracy(%)  accuracy_prop_M        Anomaly_Status   PIT_Score       Forensic_Label
#1     272.1              80.4        0.8024569    Efficiency Genius     0.967      Needs Genius Check


# 3. View the ">0.8 Outliers (+ Black Swans)" (They will appear now)
outlier_audit <- datM[datM$Forensic_Label != "System Normal", ]

print(outlier_audit)
#### method 3 
###### Add Z-score classification # check avg dist column everywhere  check tmrw

# 1. Calculate the Mean and SD of the Posterior Predictive for each golfer
ppd_mean <- apply(ppd_m, 2, mean)
ppd_sd   <- apply(ppd_m, 2, sd)

# 2. Calculate Forensic Z-Score (The 'Surprise' Unit)
# This measures how many PPD standard deviations away the actual data is
datM$Forensic_Z <- (datM$accuracy_prop_M - ppd_mean) / ppd_sd

# 3. Define the Structural Failure Bounds

library(knitr)
library(kableExtra)

# Define the Forensic Framework
forensic_table <- data.frame(
  Label = c("Black Swan: Genius", "Strong Performance", "System Normal", 
            "Structural Stress", "Black Swan: Failure"),
  `Z-Score Bound` = c("Z > 2.5", "1.8 < Z <= 2.5", "-2.0 <= Z <= 1.8", 
                      "-3.0 < Z < -2.0", "Z <= -3.0"),
  `Physical Meaning` = c("System-defying efficiency; needs 'Genius Check.'",
                         "Elite but within expected system variance.",
                         "Expected noise/nominal operation.",
                         "49% Outlier: Significant underperformance.",
                         "Mechanical/Structural collapse.")
)

# Render the Table
library(knitr)
library(kableExtra)

kable(forensic_table, 
      format = "html", 
      caption = "Forensic Z-Score Classification: PGA Accuracy Audit",
      align = "lll") %>%
  kable_styling(bootstrap_options = c("condensed"), 
                full_width = FALSE, 
                position = "left") %>%
  # Row 1: The Genius Limit
  row_spec(1, bold = TRUE, color = "white", background = "#000000") %>% 
  # Row 2: Elite Performance
  row_spec(2, bold = TRUE, color = "white", background = "#228B22") %>%
  # Row 3: Nominal Noise
  row_spec(3, bold = FALSE, color = "black", background = "#D3D3D3") %>%
  # Row 4: Your 49% "Stress" Point
  row_spec(4, bold = TRUE, color = "white", background = "#FF8C00") %>%
  # Row 5: The Failure Limit
  row_spec(5, bold = TRUE, color = "white", background = "#D7261E")

datM$Forensic_Label <- "System Normal"
datM$Forensic_Label[datM$Forensic_Z < -3.0] <- "BLACK SWAN: FAILURE"
datM$Forensic_Label[datM$Forensic_Z < -2.0 & datM$Forensic_Z >= -3.0] <- "Structural Stress"
datM$Forensic_Label[datM$Forensic_Z > 2.5]  <- "BLACK SWAN: GENIUS"
datM$Forensic_Label[datM$Forensic_Z > 1.8 & datM$Forensic_Z <= 2.5]  <- "Strong Performance" 

# 4. View the forensic  Outlier audit
audit <- datM %>% filter(Forensic_Label != "System Normal")%>%
  
print(audit[, c("Index","Avg Dist", "accuracy_prop_M", "Forensic_Z", "Forensic_Label")])
#    Avg Dist accuracy_prop_M Forensic_Z     Forensic_Label
#1    285.5       0.4900508  -2.305481    Structural Stress
#2    272.1       0.8024569   1.827152   Strong Performance

##### Inside PIT (MCMC to simulation)
# This is directly related to MCMC. When you ran stan_betareg, the MCMC sampler didn't just find one "best" line; it found 4,000 
# plausible versions of the truth (parameter sets).
# Each set of parameters contains:(beta_0 (Intercept), beta_1 (Slope for distance), phi (Precision/Noise level))
# For our golfer at x yards, the machine does this 4,000 times:
# Pick a Universe: Grab the 1st set of MCMC parameters beta_{0,1}, beta_{1,1}, phi_1).
# Calculate the Mean: Plug in the distance: mu_1 = \text{inv\_logit}(beta_{0,1} + beta_{1,1} \cdot 275)
# Roll the Dice: The Beta distribution isn't a single number; it's a "cloud." The computer draws one random accuracy value 
# (y*_1) from a Beta(mu_1, phi_1) distribution.
# Repeat: Do this for the 2nd MCMC set, the 3rd, all the way to the 4,000th.

# PIT Values are superior because 
#1. They normalize for noise: If the model expects 275-yard hits to be "chaotic" (low phi), a 10% miss won't trigger an anomaly.
#2. They detect "Too Good to be True": In a fraud audit, you aren't just looking for the failures (PIT < 0.01). You are looking for 
# the "Geniuses" who consistently cluster at PIT > 0.95. In a natural system, PIT values should be Uniformly Distributed (flat histogram). 
# If you see a spike at the high end, someone is "cooking" the data to look better than the physics allow.
# 

### mathematical breakdown of single accuracy (y*) is simulated for a golfer at a fixed distance 
#1 The transformation from line to probability

# mu_s = e^(b0,s + b1,s.x) / (1 + e^(b0,s + b1s.x)) 
# This mu_s is the Location. It tells the model where the center of the "bucket" should be for that specific iteration.

#2 The shape (From Precision to alpha & beta)
# A Beta distribution doesn't use mu and phi directly to draw a random number. It uses two shape parameters: alpha (hits) and 
# beta (misses). The computer calculates these for every single iteration:

#alpha_s = mu_s * phi_s
#beta_s = (1 - mu_s).phi_s

# 3. The "Roll of the Dice" (The Random Draw)Now the computer has a specific "cloud" shape defined by $(\alpha_s, \beta_s)$. 
# It performs a Random Sample:It reaches into that specific Beta distribution.It pulls out one single value ($y^*_s$) between $0$ and $1$.
# This is your simulated accuracy for that one universe.

# 4. Why the "Bucket Range" Varies The "range" of your 4,000 values (the width of 
# the bucket) is controlled by $\phi$ 
# (Precision):High $\phi$ (e.g., 200): The Beta distribution is a very tight spike. All 4,000 draws will be extremely close to $\mu$. 
# The "bucket" is narrow.Low $\phi$ (e.g., 10): The Beta distribution is a fat, lazy curve. The 4,000 draws will be scattered widely. 
# The "bucket" is huge.

#5. Summary: The Anatomy of a SimulationYou aren't checking "what the golfer can do"; you are checking "what 
# the model expects a golfer at this distance to do, given its current uncertainty."

# Anatomy of the simulation 
# You aren't checking "what the golfer can do"; you are checking "what the model expects a golfer at this distance to do, given its current uncertainty."
# Component,         Source,            Role
# Input (x),         Your data,         Fixed anchor for the calculation.
# MCMC (β0,β1),      Posterior Samples, "Moves the ""Center"" of the bucket 4,000 times."
# Precision (ϕ),     Posterior Samples, "Controls the ""Spread"" of the bucket 4,000 times."
# The Draw (y∗),     rbeta() function,  "The actual ""Randomness"" that creates the range."

### Method Selection Table 

# Define the Comparison Data
method_comparison <- data.frame(
  Method = c("PIT Score", "99% Bounds", "Forensic Z"),
  Strength = c("Detects cumulative probability.", 
               "Hard physical cutoff.", 
               "Measures relative intensity of surprise."),
  `Why it failed / Best` = c("Caught the 'Genius' but the 'Failure' was still too close to the 0.01 cutoff.",
                             "Useless: The 'Blurry' model made the safety net so wide that failure became 'legal'.",
                             "Best: It forces the model to acknowledge how far the 49% is from the expected mean.")
)

# Render the Comparison Table
kable(method_comparison, 
      format = "html", 
      caption = "Comparative Audit of Outlier Detection Mechanisms",
      align = "lll") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                full_width = FALSE, 
                position = "left") %>%
  # Highlight the failing 99% bounds in gray to show lack of utility
  row_spec(2, color = "#777777", italic = TRUE) %>%
  # Highlight the Forensic Z as the winner in Forest Green
  row_spec(3, bold = TRUE, color = "white", background = "#228B22")

## Visualizing Probability cloud for Black Swan Event 

#1. print(audit[, c("Avg Dist", "accuracy_prop_M", "Forensic_Z", "Forensic_Label")])


# 1. Identify the outlier (assuming it's at index 'j')
outlier_idx <- which(datM$`Drive Accuracy(%)` < 50) 
dist_outlier <- datM$`Avg Dist`[outlier_idx] #285.5
actual_acc <- datM$accuracy_prop_M[outlier_idx]
print(actual_acc) #  0.4900508

# 2. Extract Posterior Samples (4000 sets)
post_samples <- as.data.frame(bayesian_golf_beta_M_3)


beta0 <- post_samples$`(Intercept)`

beta1 <- post_samples$`\`Avg Dist\``

phi <- post_samples$`(phi)`


# 3. Calculate the Mean (mu) and Shapes (alpha, beta) for this specific distance
mu_samples <- plogis(beta0 + beta1 * dist_outlier)
print(mu_samples)

alpha_samples <- mu_samples * phi 
print(alpha_samples)
beta_samples <- (1 - mu_samples) * phi  
print(beta_samples)

# 4. Get the Median Shape for the "Expected" Distribution
alpha_med <- median(alpha_samples) # 35.2418
print(alpha_med)
beta_med <- median(beta_samples) # 19.79751

# 5. Plotting Beta pdf (Expected) and overlay the Actual performance 
library(ggplot2)
library(scales)

# Prepare the Density Data
cloud_df <- data.frame(
  Accuracy = seq(0, 1, length.out = 1000)
)
cloud_df$Density <- dbeta(cloud_df$Accuracy, alpha_med, beta_med)

# Render the Forensic Cloud
ggplot(cloud_df, aes(x = Accuracy, y = Density)) +
  # The "Expected" Reliability Envelope
  geom_area(fill = "#6a0dad", alpha = 0.2) +
  geom_line(color = "#6a0dad", size = 1.2) +
  
  # The Observed "Black Swan" (Structural Failure)
  geom_vline(xintercept = actual_acc, color = "#ff0000", linetype = "dashed", size = 1) +
  
  # Quantitative Annotation
  annotate("label", x = actual_acc, y = max(cloud_df$Density) * 0.8, 
           label = paste0("Actual: ", round(actual_acc*100, 1), "%"), 
           color = "#ff0000", fontface = "bold", fill = "white", label.size = 0.5) +
  
  # Styling for High-Definition Output
  labs(
    title = paste("Forensic Cloud: Distance =", dist_outlier, "Yards"),
    subtitle = "Posterior Predictive Beta Density vs. Actual Performance",
    x = "Accuracy Proportion",
    y = "Probability Density"
  ) +
  scale_x_continuous(labels = percent_format(), limits = c(0.2, 0.9)) + 
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    #axis.title = element_text(face = "bold")
  )
# The "structural failure" you’re seeing at 49% is a massive Probabilistic Rupture.

# The Probability Gap: The area under the curve to the left of your red dashed line (the PIT value) is effectively zero. This golfer 
# isn't just an underperformer; they are a statistical impossibility according to your model’s current internal physics.
# Link Function Stress: Your model uses a Logit link (as seen in image_01744c.png) which assumes a specific rate of decay in accuracy 
# as distance increases. A 49% accuracy at 285.5 yards indicates a "structural collapse" where the standard trade-off between power 
# and precision has been violated.

# Leverage on the Mean: In image_01744c.png, the points falling off the line at the bottom-left represent this event. This "Black Swan"
# is likely single-handedly dragging your regression line (the S-Curve in image_017126.png) away from the "consensus" data.The GAMLSS 
# Diagnostic: Your Beta GAMLSS plot (image_00f12b.png) shows a tighter fit, which suggests that the "structural failure" isn't just 
# in the mean accuracy, but in the Dispersion ($\phi$). This golfer is a symptom of Heteroscedasticity—the system's "noise" is not constant.

#### PIT Histogram 

# The PIT (Probability Integral Transform) Histogram is the ultimate diagnostic for Bayesian calibration. 
# If your model "understands" the system’s physics, the PIT values must be Uniformly distributed U(0, 1).

# Posterior Predictive Samples 
y_rep = ppd_m 
y_obs = datM$accuracy_prop_M

library(bayesplot)

# y_obs: vector of actual accuracy (%)
# y_rep: S x N matrix of posterior predictive samples
ppc_pit_ecdf(y_obs, y_rep) + 
  ggtitle("In-Sample PIT: PGA Model Calibration")
# Method 1 (The ECDF Plot): This is technically "right" but visually deceptive for a forensic seismology or data science expert. 
# While it shows the model is roughly within the confidence bands, it "smoothes over" the specific clusters of failure.

# Manual Method (I) 
# This is the winner. By plotting the PIT values from $0$ to $1$, it reveals the Over-dispersion (Humped Shape).
# This "Bell Curve" shape in a PIT histogram is the mathematical proof of Over-dispersion.
# The Reality: Your PGA golfers are more consistent than your model thinks they are.
# The Model's Flaw: Your Beta distribution is overestimating the "noise" in the system. It thinks the variance is huge, so when a golfer hits a 49% accuracy, the model isn't "surprised" enough. It treats a structural failure like a normal Tuesday.
# The Evidence: Your Q-Q plots (the red and green lines in your uploads) show the residuals hugging the line, but the PIT hump confirms that the model is "playing it safe" by over-predicting the spread of outcomes.

# Calculate PIT values: Proportion of y_rep <= y_obs
pit_values_1 <- colMeans(apply(y_rep, 1, function(s) s <= y_obs))

ggplot(data.frame(pit = pit_values), aes(x = pit)) +
  geom_histogram(bins = 20, fill = "#6a0dad", color = "white", boundary = 0) +
  theme_minimal() +
  labs(title = "Manual In-Sample PIT Histogram",
       x = "Probability Integral Transform",
       y = "Frequency")

# Method 2 (The Tight Histogram): This looks "wrong" because the x-axis is truncated (0.4 to 0.6). It hides the tails. 
# In Bayesian calibration, the PIT values must span the full $[0, 1]$ interval to be valid.

# Manual PIT: What % of samples are <= actual observed data?
pit_values_2 <- colMeans(apply(ppd_m, 1, function(s) s <= datM$accuracy_prop_M))

# Plotting the "Truth"
ggplot(data.frame(pit = pit_values_2), aes(x = pit)) +
  geom_histogram(breaks = seq(0, 1, by = 0.05), fill = "purple4", color = "white") +
  xlim(0, 1) + # Critical: Must show the full 0 to 1 range
  theme_minimal() +
  labs(title = "Forensic PIT: Over-dispersed Beta Model",
       subtitle = "Humped shape = Model overestimates system noise")


########## Weighted Histogram 
library(loo)
library(ggplot2)
# Calculate Log-Likelihoods for both models
loglik_Bay <- log_lik(bayesian_golf_beta_M_3)
print(loglik_Bay)
#loglik_beta <- log_lik(fit_beta_var)

# Use LOO (Leave-One-Out Cross-Validation) to compare
# This is a proxy for KL Divergence in Bayesian workflows

#loo_gaussian <- loo(bayesian_golf)
#loo_beta <- loo(fit_beta_var)

# Compare them: If elpd_diff is negative, the Beta model is the 'superior truth'
#comparison <- loo_compare(loo_gaussian, loo_beta)
#print(comparison)

# 1. Compute PSIS weights from your existing log-likelihood matrix
# This handles the numerical stability for your 'Black Swan' outliers
psis_m <- psis(loglik_Bay)
print(psis_m) #

lw_m <- weights(psis_m, log = FALSE) # Normalized weights
print(lw_m)

# 2. Manual Weighted PIT Calculation
# We calculate the cumulative probability for each observation 'i'
# across all posterior samples 's', weighted by the importance of each sample.
n_obs <- ncol(ppd_m)
pit_weighted <- numeric(n_obs)

for (i in 1:n_obs) {
  # For each golfer, find which samples are below the actual accuracy
  indicator <- ppd_m[, i] <= datM$accuracy_prop_M[i]
  # Weight those samples by the PSIS weights for that specific observation
  pit_weighted[i] <- sum(indicator * lw_m[, i]) / sum(lw_m[, i])
}

# 3. Plot the Clean Weighted Histogram
ggplot(data.frame(pit = pit_weighted), aes(x = pit)) +
  geom_histogram(breaks = seq(0, 1, by = 0.05), 
                 fill = "#6a0dad", color = "white", boundary = 0) +
  geom_hline(yintercept = n_obs / 20, linetype = "dashed", color = "red") + 
  xlim(0, 1) +
  theme_minimal() +
  labs(title = "LOO-Weighted PIT Histogram: PGA Structural Check",
       subtitle = "Dashed line represents perfect calibration (Uniform Distribution)",
       x = "Weighted Probability Integral Transform",
       y = "Frequency")


# Verified Over dispersed model (method 3 vs weighted psis)
# It is predicting a wider range of accuracy for each distance than what actually occurs in the real world. It's "too cautious"—
# expecting more volatility than the PGA golfers are actually providing.
# The Black Swan Impact: Because the model assumes the system is noisier than it is, it "normalizes" outliers. In your first two 
# plots you can see the 49% point sitting in the tail, but because the distribution is too wide, 
# the model's "surprise" is mathematically dampened.
# If your pareto_k values are $> 1$, it means your $49\%$ accuracy point is so far outside the model's expectations that the 
# Importance Sampling failed. The math literally "broke" trying to bridge the gap between your model and that specific golfer.

# Why "All Pareto k < 0.7" matters
# The fact that your k values are all in the "good" range is a huge win for your forensic modeling. 
# It proves that:Stable Logic: Your model’s posterior isn’t being dominated by any single "rogue" golfer.Valid Weights: 
# The weights you used to create the final histogram are mathematically reliable.True Structural Flaw: The hump isn't an artifact of 
# a "broken" calculation—it is a valid signal that your model's precision parameter ($\phi$) is currently miscalibrated.

# $k < 0.5$: The model handles the data point well.
# $0.5 < k < 0.7$: The point is an outlier, but the LOO approximation is still holding.
# $k > 0.7$: Structural Failure. This point is so influential or "weird" that the Beta distribution cannot reconcile it. 
# If your $49\%$ point has $k > 0.7$, you have mathematical proof that a standard Beta model is insufficient for forensic seismic-
# level precision in this circuit.

plot(psis_m)
# The PSIS diagnostic plot you generated is the "Clean Bill of Health" for your Bayesian model's structural integrity. 
# In forensic data science, this is the proof that your "Black Swan" isn't a mathematical glitch, but a legitimate outlier your model 
# can actually digest.

# Verdict : System Limitation. You have successfully built a model that can digest extreme outliers without crashing ($k < 0.7$), 
# but sacrificed resolution.

#1. Safe psis is deceptive 
#2. The Irony: Because Beta model is over-dispersed (the "Humped" PIT), every single point is seen as "plausible".
#3. The Result: If the model thinks anything is possible, then nothing is surprising. That is why your k values are so low—the 
#   49% golfer isn't "breaking" the model because the model has already "pre-surrendered" by predicting massive noise.
################################################
# Cases where it fails (Even with Fat-Tail Beta)
# Even with k < 0.7, the system fails in these four "Closed Loop" scenarios:

#1. The Over Dispersion Paradox (The Lazy Model)
# Failure: The model correctly "digests" the 49% outlier, but its predictive intervals are so wide ($30\%$ to $90\%$) that they are 
# useless for decision-making.Sign: A massive hump in the center of your PIT histogram.

#2. Boundary Saturation (The 0/1 Wall)Failure: Beta distributions struggle as accuracy approaches 100% or 0%. If a golfer hits 99% 
# accuracy at 300 yards, the Beta model may fail to reconcile the "physical impossibility" of that precision, leading to biased 
# estimates despite "good" k values.

#3.Non-Linear Structural Breaks
# Failure: If there is a "tipping point" (e.g., after 290 yards, mechanics fundamentally change), the Beta model (which assumes a 
# smooth transition via the logit link) will stay "safe" by increasing variance rather than changing the mean.
# Sign: Your PSIS crosses might be stable, but your residuals will show a specific "U-trend" against distance.

#4.Information Asymmetry 
# Failure: The system is "closed" because it only looks at distance and accuracy. If the 49% failure was due to a specific "Black 
# Swan" event (e.g., wind, injury), the model has no way to "see" it, so it just inflates the "error" term (phi) for everyone.

###  Champion Outlier : High K search 
# Validity: High (Diagnostic). Even though all k values are < 0.5, identifying the "highest of the low" is a forensic necessity.
# Why it matters: If the golfer with the highest k is also the one with the most extreme distance (e.g., your 285-yard case), it 
# confirms that the structural uncertainty of your model is localized at the boundaries of the physics.
# The Goal: We are looking for the point where the "Importance Sampling" is working the hardest to keep the model from crashing.


# 1. Extract k-values and find the 'champion' of outliers
k_values <- pareto_k_values(psis_m)
max_k_idx <- which.max(k_values)
print(max_k_idx) # id  = 124, k value = 0.141007

print(datM)
outlier_report <- datM[max_k_idx, ]

print(paste("The structural limit of the model is being tested at Row:", max_k_idx))
print(outlier_report) 
#     avg_dist drive_accuracy_pct accuracy_prop_M Anomaly_Status PIT_Score Forensic_Label
#281      275               65.5       0.6542132  System Normal   0.34375  System Normal
print(paste("Pareto k for this point:", round(k_values[max_k_idx], 4)))
# Pareto k for this point: 0.141


# Pareto K paradox 
# "Black Swan" (Row 207), but your code identified Row 124 (which corresponds to your printed index 281).
# Row 207 (The Actual Outlier): avg_dist: 285.5, accuracy: 0.49.
# Row 124/281 (The "Max $k$" Point): avg_dist: 275, accuracy: 0.65.

# Because Pareto k doesn't measure "how far" a point is from the mean; it measures how much that point influences the shape of the 
# distribution. In your current "Lazy" model, the global precision (phi) is so wide that the 49% accuracy at 285 yards is seen as 
# "statistically boring"—the model has enough "fat" in its tails to swallow it without blinking.

###### Actual Outlier 
# 207  285.5  49.0  0.4900508  Structural Failure  0.01200  System Normal

# PIT Score 0.012: This is the smoking gun. A PIT of 0.012 means this golfer is in the bottom 1.2% of predicted outcomes.
# The Conflict: You labeled it "Structural Failure," but your Forensic_Label (based on the model) called it "System Normal".

# This is the Systemic Ceiling. Your model is so over-dispersed (the "Hump" in the PIT) that even a 1.2% probability event is 
# classified as "Normal." The model is essentially saying: "I'm so unsure about everything that I refuse to call anything an anomaly."


# 2. Highlight this golfer in your Distance vs Accuracy space
ggplot(datM, aes(x = `Avg Dist`, y = accuracy_prop_M)) +
  geom_point(alpha = 0.4, color = "black", ) +
  # Highlight the High-k point
  geom_point(data = datM[max_k_idx, ], color = "red", size = 4, shape = 18) +
  geom_text(data = datM[max_k_idx, ], aes(label = paste0("Max k: ", round(k_values[max_k_idx], 3))), 
            vjust = -1, color = "red", fontface = "bold") +
  theme_minimal() +
  labs(title = "Structural Strain Map",
       subtitle = "Red point indicates the golfer pushing the Beta distribution to its limit",
       xlabel = "")

# Modeling phi ( Heteroskedasticity )
# 1. Current Beta model (Low Res) assumes that the "noise" (precision phi) is a constant. But in reality, the physics of a 200-yard 
# shot are much more stable than a 300-yard shot.

# 2. By modeling phi ~ distance, you allow the model to be "High-Resolution" for shorter distances and "Forensic/Cautious" only where 
# the data actually gets volatile. This forces the model to treat the 49% accuracy point at 285 yards as a specific failure of 
# precision rather than just a "normal" part of a wide, lazy distribution.
# 
## sharpening resolution using HD Beta model 

#1.Resolution Increases: The model realizes that at 285 yards, the "Normal" accuracy range should be tighter.
#2.k-values Shift: Suddenly, the 49% point (Row 207) will become "expensive" for the model to explain. Its k-value will spike because 
# it can no longer be "swallowed" by a lazy, wide distribution.
#3.Anomaly Detection Flips: The Forensic_Label for Row 207 will likely flip from "System Normal" to "Anomaly" because the PIT score 
# will drop even further, or the predictive interval will shrink.


# Get the row index specifically for your 49% golfer (Actual Outlier)
# Extract the data the model actually 'saw'

swan_idx = 207

# Now calculate the surprise (Log-Predictive Density)
loglik_vals <- log_lik(bayesian_golf_beta_M_3)

# Extract all 4000 samples for the Black Swan (Column 207)
swan_samples <- loglik_vals[, "207"]

# To see the mean "Surprise" (Log-Predictive Density)
swan_surprise = mean(swan_samples)  #-0.8006965
print(paste("Internal Index:", swan_idx)) 
print(paste("Surprise Score:", round(swan_surprise, 4))) # -0.8007

# To see the distribution of how 'surprised' the model is
hist(swan_samples, 
     main="Log-Likelihood Distribution: Black Swan", 
     col="pink", 
     breaks = 50,  # Increase this for more resolution
     xlab = "Log-Likelihood (Surprise)",
     border = "white")
# Add a line at the mean to see the center clearly
abline(v = mean(swan_samples), col = "red", lwd = 2, lty = 2)

# Sensitivity Gap 
# Global Gap 
# Get the average 'comfort' of the model
avg_loglik <- mean(loglik_vals) # 1.59148

# Get the 'stress' of the Black Swan
swan_loglik <- mean(loglik_vals[, "207"]) # -0.8006965

# The Gap: If this is small, your model is LOW RESOLUTION
print(avg_loglik - swan_loglik) #2.392177


# Systemic Ceiling 
# Use the Specific Gap (2.49) for your portfolio. It highlights the maximum contrast, which makes for a more compelling argument that the model's resolution is capped.
# Compare the "Normal" golfer (Index 158) vs the "Black Swan" (Index 207)
mean_normal <- mean(loglik_vals[, "158"]) # or index 1
mean_swan   <- mean(loglik_vals[, "207"])

# The Delta (The 'Sensitivity' of your current model)
sensitivity_gap <- mean_normal - mean_swan
print(sensitivity_gap) #2.489487
# If your sensitivity_gap is small (less than 3.0), your model is Low-Resolution. It isn't penalizing the "Black Swan" enough. This is exactly why your PIT histogram has a hump—the model is treating a massive failure (49%) as only "slightly less likely" than a perfect hit.
# Create the comparison dataframe
plot_df <- data.frame(
  Golfer = c("Normal (158)", "Black Swan (207)"),
  LogLik = c(mean_normal, mean_swan)
)
ggplot(plot_df, aes(x = Golfer, y = LogLik, fill = Golfer)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = round(LogLik, 3)), vjust = -0.5) +
  theme_minimal() +
  labs(title = "The Systemic Ceiling: Low-Resolution Comparison",
       subtitle = paste("Sensitivity Gap:", round(sensitivity_gap, 3)),
       y = "Mean Log-Likelihood") +
  scale_fill_manual(values = c("red", "gray60"))

# Comment on Resolution 
# In Bayesian model comparison and information theory, a difference of 3.0 in log-units is a standard "Rule of Thumb" for significant 
# evidence. It is derived from the Kullback-Leibler (KL) Divergence and the scale of Log-Odds.

# Log-Likelihood Difference < 2: Considered "Weak" or "Barely Worth Mentioning."

# Log-Likelihood Difference of 2 to 3: "Positive" evidence, but not overwhelming. This is where you are now (2.49).

# Log-Likelihood Difference > 3: "Strong" evidence. This is the threshold where a data scientist can confidently say, "This point is fundamentally different from the rest of the system."

# By staying below 3.0, your model is essentially "mumbling" that the Black Swan might be an outlier, rather than "shouting" it.


#1. gap of 2.39 is mediocre.Since log-likelihood is on a log scale, a difference of 2.39 means the model finds the "Normal" golfer 
# about e^{2.39} ~ 11 times more likely than the "Black Swan.
#2."The High-Resolution Standard: For a 49% accuracy failure at a structural limit (285 yards), an expert-level model should show a 
# gap of 5.0 to 10.0 (e^5~ 148 times more likely).
#3. Why 2.39 is "Blurry": Your current model is "gaslighting" the data by using a global phi = 55. It is forcing the Black Swan 
# to be "almost normal" just to keep the math simple.

# Model Type,         Gap Score,   Meaning
# Current (Lazy ϕ),   2.49,        "Low Resolution: Outlier is ""tolerated.""" 
# Target (Dynamic ϕ), > 5.0,       "High Definition: Outlier is ""exposed.""" 

#### Log-likelihood Cliff 
# It visualizes the "Systemic Blindness" by showing how the model's comfort drops off as it encounters  outlier.
# 1. Calculate Mean Log-Lik for every golfer in the model
all_surprises <- colMeans(loglik_vals)

# 2. Create a data frame for plotting
plot_data <- data.frame(
  Internal_Index = 1:length(all_surprises),
  Surprise = all_surprises,
  Is_Swan = (colnames(loglik_vals) == "207")
)

# 3. Plot the 'Cliff'
ggplot(plot_data, aes(x = Internal_Index, y = Surprise, color = Is_Swan)) +
  geom_point(alpha = 0.7, size = 3, shape = 18) +
  geom_hline(yintercept = avg_loglik, linetype = "dashed", color = "gray40", size = 0.8) +
  scale_color_manual(values = c("black", "red")) +
  scale_y_continuous(breaks = c(-1, -0.8, 0,1, 1.59))+
  labs(title = "Log-Likelihood Cliff",
       subtitle = "Red point indicates the Black Swan (Row 207) | Sensitivity Gap: 2.489\nSystemic Blindness : model's comfort drops off as it encounters an outlier ",
       x = "Golfer Index (Model Internal)",
       y = "Mean Log-Likelihood") +
  theme_minimal()

# Overlay this with Beta HD to show the X % improvement in modeling

##### Verdict
# "I identified a 'blur' in the baseline Bayesian regression where a constant precision parameter ($\phi$) caused the model to be 
# under-sensitive to structural failures at high distances. The model's surprise score for a 49% accuracy event was only -0.75—statistically 
# negligible."

# again systemic limit 
# Black Swan Internal Index 207 : -0.6718, -0.7805, -0.6622, -0.7274, -0.9396.
# These are the only negative values in that entire block of text. While every other golfer (like 160 or 210) has a log-likelihood 
# around +1.8, the Black Swan is down in the negatives.
# In Bayesian terms, a log-likelihood of -0.7 isn't a "scream"; it’s a "whisper." A true outlier in a high-precision model should 
# have a log-likelihood of -5.0 or -10.0. The fact that it's only slightly negative proves the model is using that "Lazy" global 
# $\phi=55$ to buffer the failure.

# Systemic Normal Illusion 
#1. Check index 165 (which your datM labeled "Efficiency Genius"):
# Values: 1.03, 1.27, 1.12.
#2.The Comparison: Your "Black Swan" (207) has a score of -0.7, and your "Genius" (165) has a score of +1.1.
# The Gap: The mathematical distance between a "Structural Failure" and an "Efficiency Genius" is only about 1.8 units. This gap is 
# too small. This is the Systemic Ceiling—the model is compressing all human performance into a narrow band because it can't 
# distinguish between skill and noise.

#################################################################################

# Precision Resolution plot 
# This is the "Focus Adjustment". If you model phi as a function of distance, you can plot the Model's Certainty across the 
# yardage gradient.

# Low Res 
# Logic to visualize why the PIT is humped
# We compare the 'Global' phi (current) vs a 'Dynamic' phi (needed)
# Get the point estimate for phi

### Clean column names to avoid back tick hell 
#colnames(datM)[colnames(datM) == "Avg Dist"] <- "avg_dist"
#colnames(datM)[colnames(datM) == "Drive Accuracy(%)"] <- "drive_accuracy_pct"

phi_global <- bayesian_golf_beta_M_3$phi # Assuming brms/stan
print(phi_global) # 55.03805

ggplot(datM, aes(x = `Avg Dist`)) +
  # Current 'Lazy' Model
  geom_hline(aes(yintercept = phi_global, color = "Global Phi\nConstant (Blurry)"), 
             linetype = "dashed", size = 0.8) +
  
  # Structural Boundaries
  geom_vline(xintercept = c(270, 285, 290, 310), color = "red", 
             linetype = "dashed", alpha = 0.5, size = 0.6) +
  
  # The 'High-Definition' Path
  geom_smooth(aes(y = 1/abs(accuracy_prop_M - mean(accuracy_prop_M)), 
                  color = "Dynamic (HD-Path)"), 
              method = "loess", se = TRUE, alpha = 0.2) + 
  
  # Region Annotations
  annotate("text", x = 265, y = 600, label = "The Fog", angle = 90, size = 3.5, color = "gray40") +
  annotate("text", x = 277, y = 600, label = "Blind Spot", angle = 90, size = 3.5, color = "gray40") +
  annotate("text", x = 287.5, y = 600, label = "Precision Peak", angle = 90, size = 3.5, color = "purple") +
  annotate("text", x = 300, y = 600, label = "Efficiency Zone", angle = 90, size = 3.5, color = "gray40") +
  annotate("text", x = 315, y = 600, label = "Entropy Zone", angle = 90, size = 3.5, color = "gray40") +
  
  # Aesthetic Mapping
  scale_color_manual(name = "Model Resolution", 
                     values = c("Global Phi\nConstant (Blurry)" = "blue", 
                                "Dynamic (HD-Path)" = "purple")) +
  scale_x_continuous(breaks = c(260, 270, 285, 290, 300, 310)) +
  
  theme_minimal() +
  labs(title = "Required Resolution vs. Current Resolution Decision Plot",
       subtitle = "Purple Peak at 287y unmasks the 'Black Swan' Resolution Deficit\nGlobal Phi: 55.04|Required Phi at Swan:156.71| Deficit Factor:2.85",
       y = "Precision (Phi)", 
       x = "Distance (Yards)") +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())
# Regions 
#1. 260–270 (The Fog): High epistemic uncertainty (wide SE ribbon). The model is guessing due to low data density.
#2. 270–285 (The Blind Spot): The constant model is actually over-resolved here. It's assuming more precision than the data suggests 
##.is necessary.
#3. 285–290 (The Precision Peak): This is the "Black Swan" territory. The data demands phi ~ 200, but your model is capped 
# at 55. This is where the Resolution Deficit is maxed out.
#4.290–310 (The Efficiency Zone): Normal operating range where the model's "blurry" resolution is actually close to the dynamic 
## requirement.
#5.310+ (The Entropy Zone): Precision requirements collapse as the distance approaches the limit of the physical system.

# The Diagnostic: The constant model (Blue) assumes a uniform system precision, while the dynamic path (Purple) reveals a massive 
# spike in required precision at 285–290 yards.

# The Systematic Failure: Because the blue line sits far below the purple peak, the model lacks the "resolution" to recognize the 
# 49% accuracy golfer as a true anomaly.

# The Recommendation: To break this "Systemic Ceiling," we must transition to a Heteroskedastic Beta Regression (HD Model), allowing 
# the model's surprise sensitivity to scale with the physical demands of the distance.

# Where the purple line is far above the blue line, your model is under-confident (too much noise). Where it's below, your model is 
# over-confident. This mismatch is exactly what creates the "Hump" in your PIT histogram because the probability residuals aren't 
# being distributed correctly.  
# The Black Swan Blindness: At the 285-yard mark, if the Blue line stays far from the required Purple precision, the model fails to 
# "punish" the 49% accuracy score, resulting in the mediocre Sensitivity Gap of 2.49 you calculated.

##### Quantifying Deficit 
# Predict required phi at 285 yards using the loess model
loess_fit <- loess(1/abs(accuracy_prop_M - mean(accuracy_prop_M)) ~ `Avg Dist`, data = datM)
required_phi_at_swan <- predict(loess_fit, newdata = data.frame(`Avg Dist` = 285))
print(required_phi_at_swan) #156.7079 

# Calculate the Deficit Factor
deficit_factor <- required_phi_at_swan / phi_global
print(paste("The model is under-resolved by a factor of:", round(deficit_factor, 2)))
# "The model is under-resolved by a factor of: 2.85" 

### Prior Predictive Check 
# by refitting the model while ignoring the likelihood (sampling only from the prior), your priors were already "lazy" before they 
# even saw the 49% golfer.

# Refit the model sampling only from the priors
prior_only_model <- update(bayesian_golf_beta_M_3, prior_PD = TRUE)

# Run the check

y_rep_prior <- posterior_predict(prior_only_model)
ppc_dens_overlay(datM$accuracy_prop_M, y_rep_prior[1:50, ]) + 
  ggtitle("Prior Predictive Check: Are your priors too blurry?")



# High-Definition Beta Model

library(brms)
### Clean column names to avoid back tick hell 
colnames(datM)[colnames(datM) == "Avg Dist"] <- "avg_dist"
colnames(datM)[colnames(datM) == "Drive Accuracy(%)"] <- "drive_accuracy_pct"

# 2. Run the High-Definition Model with clean names
fit_Betahd <- brm(
  bf(accuracy_prop_M ~ avg_dist, 
     phi ~ avg_dist),             
  data = datM,  
  family = Beta(),
  cores = 4,  
  iter = 2000
)

