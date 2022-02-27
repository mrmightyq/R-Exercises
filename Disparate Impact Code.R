# Create cross-tabs table of observed data
observed <- xtabs(~ Ethnicity + Selected, data=df)

# Print cross-tabs table of observed data
print(observed)

# Create pass and fail objects for men containing the raw frequencies/counts
pass_men <- observed[1,2]
fail_men <- observed[1,1]


# Create an object that contains the total number of men who participated in the test
N_men <- pass_men + fail_men

# Create object containing number of women who passed, number of women who failed,
# and the total number of women who participated in the test
pass_women <- observed[2,2]
fail_women <- observed[2,1]
N_women <- pass_women + fail_women

# Create object containing selection ratio for men
SR_men <- pass_men / N_men

# Create object containing selection ratio for women
SR_women <- pass_women / N_women

# Create impact ratio object
IR <- SR_women / SR_men

# Print impact ratio object
print(IR)


# Apply "flip-flop rule" (impact ratio adjusted)
IR_adj <- ((pass_women + 1) / N_women) / ((pass_men - 1) / N_men)
print(IR_adj)

# View row and column marginals for table
addmargins(observed)

# Convert table object values to proportions by Gender (columns)
prop_observed <- proportions(observed, "Ethnicity")

# Compute selection ratios for men
SR_men <- prop_observed[1,2]

# Compute selection ratios for women
SR_women <- prop_observed[2,2]

# Compute impact ratio (IR)
IR <- SR_women / SR_men

# Print impact ratio (IR)
print(IR)


# Compute chi-square test of independence
chisq.test(observed, correct=FALSE)


# View observed values
chisq.test(observed, correct=FALSE)$observed

# View expected values
chisq.test(observed, correct=FALSE)$expected


# Compute chi-square test of independence (with Yates continuity correction)
chisq.test(observed, correct=TRUE)


# Access the psych package
library(psych)

# Compute phi coefficient
phi(observed)


# Compute fisher test
fisher.test(observed)

# Create object for the total number of men
N_men <- sum(observed[1,])

# Create object for the total number of women
N_women <- sum(observed[2,])


# Calculate marginal (overall, total) selection ratio/rate
SR_total <- sum(observed[,2]) / sum(observed)


# Convert contingency table to proportions by row
prop_observed <- prop.table(observed, 1)

# Compute selection ratios/rates (SR)
SR_men <- prop_observed[1,2]
SR_women <- prop_observed[2,2]


# Compute Z-value
Z <- (SR_women - SR_men) / 
  sqrt(
    (SR_total*(1 - SR_total) * ((1/N_women) + (1/N_men)))
  )


# Compute absolute value of Z-value
abs(Z)

# Calculate exact p-value (one-tailed)
pnorm(abs(Z), lower.tail=FALSE)


# Calculate exact p-value (two-tailed)
2*pnorm(abs(Z), lower.tail=FALSE)

# Compute Z-value
Z_IR <- log(SR_women/SR_men) / 
  sqrt(((1 - SR_total)/SR_total) * (1/N_women + 1/N_men))


# Compute absolute value of Z-value
abs(Z_IR)

# Calculate exact p-value (one-tailed)
pnorm(abs(Z_IR), lower.tail=FALSE)

# Calculate exact p-value (two-tailed)
2*pnorm(abs(Z_IR), lower.tail=FALSE)

# Compute natural log of impact ratio (IR)
IR_log <- log(SR_women/SR_men)

# Compute standard error of IR (SE_IR)
SE_IR <- sqrt(
  ((1 - SR_women) / 
     (N_women * SR_women) + (1 - SR_men)/(N_men * SR_men))
)


# Compute bounds of 95% confidence interval for natural log
# of impact ratio
LCL_log <- IR_log - 1.96 * SE_IR # lower
UCL_log <- IR_log + 1.96 * SE_IR # upper


# Convert to scale of original IR metric
LCL <- exp(LCL_log) # lower
UCL <- exp(UCL_log) # upper

# Print the 95% confidence intervals
print(LCL)
print(UCL)

# Compute bounds of 90% confidence interval for natural log
# of impact ratio
LCL_log <- IR_log - 1.645 * SE_IR # lower
UCL_log <- IR_log + 1.645 * SE_IR # upper

# Convert to scale of original IR metric
LCL <- exp(LCL_log) # lower
UCL <- exp(UCL_log) # upper

# Print the 90% confidence intervals
print(LCL)


print(UCL)
