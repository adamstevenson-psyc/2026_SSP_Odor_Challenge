# =====================================================
# Load packages
# =====================================================
library(dplyr)
library(tidyr)
library(pROC)
library(boot)
library(ggplot2)


# =====================================================
# Read data
# =====================================================
dat <- read.csv("SSP_Workshop_Raw_Data.csv")
# View(SSP_Workshop_Raw_Data)


# Attribute of interest
attr <- "OffNote1"


# =====================================================
# Split into products
# =====================================================
masked   <- dat %>% filter(Product == "Masked") %>% pull(OffNote1)
unmasked <- dat %>% filter(Product == "Unmasked") %>% pull(OffNote1)


# =====================================================
# 1. ANOVA / Mixed Blocking Approach
# =====================================================
fit <- aov(OffNote1 ~ Product + Panelist + factor(Rep),
           data = dat)
summary(fit)


# =====================================================
# 2. Paired Comparison by Panelist x Rep
# =====================================================
paired_dat <- dat %>%
  select(Panelist, Rep, Product, OffNote1) %>%
  pivot_wider(names_from = Product,
              values_from = OffNote1)


t.test(paired_dat$Unmasked,
       paired_dat$Masked,
       paired = TRUE)


wilcox.test(paired_dat$Unmasked,
            paired_dat$Masked,
            paired = TRUE)


# =====================================================
# 3. Parametric Signal Detection Sensitivity (d-prime)
# =====================================================
sp <- sqrt(
  ((length(masked)-1)*var(masked) +
     (length(unmasked)-1)*var(unmasked)) /
    (length(masked)+length(unmasked)-2)
)


d_prime <- (mean(unmasked) - mean(masked))/sp


d_prime


# implied ROC area assuming equal variance SDT
auc_parametric <- pnorm(d_prime / sqrt(2))
auc_parametric


# =====================================================
# 4. Parametric Signal Detection Bias (C)
# =====================================================


# Define a decision threshold (cutoff) on your OffNote1 scale.
# Ratings >= cutoff are treated as "Signal Present" responses.
# (Adjust this value based on your actual sensory scale, e.g., 5 on a 9-point scale)cutoff <- 1
#cutoff <- 1
#H <- mean(unmasked >= cutoff)
#F_rate <- mean(masked >= cutoff)




# Apply a standard correction (e.g., Macmillan & Creelman) to avoid 0 or 1.
# 0 or 1 will result in infinite Z-scores in qnorm().
# Here we use a simple bound, but you can also use (count + 0.5) / (n + 1)
#epsilon <- 0.001
#H <- max(min(H, 1 - epsilon), epsilon)
#F_rate <- max(min(F_rate, 1 - epsilon), epsilon)




# Parametric Criterion (C)
# Negative values = liberal bias (tendency to say yes), Positive = conservative bias
#c_bias <- -0.5 * (qnorm(H) + qnorm(F_rate))


#c_bias


# =====================================================
# 5. ROC Analysis
# =====================================================
roc_obj <- roc(response = dat$Product,
               predictor = dat$OffNote1,
               levels = c("Masked","Unmasked"))


plot(roc_obj)
auc(roc_obj)


# =====================================================
# 6. Non-parametric A-prime
# =====================================================
A_prime <- as.numeric(auc(roc_obj))
A_prime


# =====================================================
# 7. Bootstrap Confidence Interval for A-prime
# =====================================================
boot_auc <- function(data, indices){
  
  d <- data[indices, ]
  
  roc_obj <- roc(response = d$Product,
                 predictor = d$OffNote1,
                 levels = c("Masked","Unmasked"),
                 quiet = TRUE)
  
  as.numeric(auc(roc_obj))
}


set.seed(123)
boot_results <- boot(dat,
                     statistic = boot_auc,
                     R = 10000)


boot.ci(boot_results,
        type = c("perc","bca"))


# =====================================================
# 8. Figures for Presentation
# =====================================================




# Intensity plot


ggplot(dat,
       aes(Product, OffNote1,
           color = Product)) +
  geom_jitter(width = 0.08) +
  stat_summary(fun = mean,
               geom = "point",
               size = 4,
               color = "black") +
  theme_minimal()


# Set the plot region to be square
par(pty = "s")


# ROC Plot
plot(roc_obj,
     print.auc = TRUE,
     col = "blue",
     lwd = 3)
abline(a = 0,
       b = 1,
       lty = 2)