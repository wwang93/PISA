
# Load libraries
library(EdSurvey)
library(dplyr)
library(ggplot2)
library(lme4)

# Wei, you'll need to change this to point to your Google drive folder
#data_dir <- "G:\\.shortcut-targets-by-id\\1sq4hwqg5b3AtiOdPFn52VqsjgR4p22GO\\PISA_ICT_Project( Wei and Louis)\\PISA_0809_dataset\\PISA\\2022"

data_dir <- "G:\\My Drive\\PISA_ICT_Project( Wei and Louis)\\PISA_0809_dataset\\PISA\\2022"


# read in PISA USA data
# To read in other countries add them to the countries list
pisa <- readPISA(path = data_dir, countries = "USA")

# save out the codebook to ensure we have the right variables
codebook <- showCodebook(pisa)
write.csv(codebook, "codebook_usa.csv")

# Subset the data keeping only variables we are using
pisa_dat <- EdSurvey::getData(pisa, varnames = c("cnt", "w_fstuwt", "w_schgrnrabwt", "st004d01t","cntschid", "cntstuid", "oecd", "age", "misssc", "workpay", "immig", "bsmj", "relatst", "learres", "famsupsl", "hisced", "hisei", "homepos", "ictsch", "icthome", "ictqual", "ictout", "ictwkdy", "icteffic", "ictenq", "ictfeed", "pv1math", "pv2math", "pv3math", "pv4math", "pv5math", "pv6math", "pv7math", "pv8math", "pv9math", "pv10math", "pv1read", "pv2read", "pv3read", "pv4read", "pv5read", "pv6read", "pv7read", "pv8read", "pv9read", "pv10read", "pv1scie", "pv2scie", "pv3scie", "pv4scie", "pv5scie", "pv6scie", "pv7scie", "pv8scie", "pv9scie", "pv10scie"), addAttributes = TRUE)


#write.csv(pisa_dat, "pisa_usa_data.csv")
write.csv(pisa_dat, "pisa_usa_data.csv")



# Run frequencies on all variables to check how they are coded
# List of variables to loop through for frequencies 
variables <- c("st004d01t", "cntschid", "age", "misssc", "workpay", 
               "immig", "bsmj", "relatst", "learres", "famsupsl", "hisced", "hisei", 
               "homepos", "ictsch", "icthome", "ictqual", "ictout", "ictwkdy", 
               "icteffic", "ictenq", "ictfeed")

# Loop through each variable and print the frequency table if there are 20 or fewer categories
for (var in variables) {
  # Count the number of unique categories
  num_categories <- length(unique(pisa_dat[[var]]))
  
  # Check if the number of categories is less than or equal to 20
  if (num_categories <= 20) {
    # Print variable name for clarity
    cat("\nFrequency table for", var, ":\n")
    
    # Generate and print the frequency table using janitor::tabyl
    print(janitor::tabyl(pisa_dat[[var]]))
  } else {
    cat("\nSkipping variable", var, "because it has more than 20 categories.\n")
  }
}

# Recode workpay into no work and working for pay at least one day a week
pisa_dat$workpay_recode <- ifelse(pisa_dat$workpay == "NO WORK FOR PAY", 0, 1)

# Recode immig into immigrant vs native student
pisa_dat$immig_recode <- ifelse(pisa_dat$immig == "NATIVE STUDENT", 0, 1)

# Verify recoding
janitor::tabyl(pisa_dat$workpay_recode)
janitor::tabyl(pisa_dat$immig_recode)

# Ensuring that School students attend is a factor variable since we are going
# to use school as a fixed effect in the model
pisa_dat$cntschid <- as.factor(pisa_dat$cntschid)

# define regression formula  
formula <- as.formula("math ~ st004d01t + age + immig_recode + workpay_recode + misssc + 
                      bsmj + relatst + learres +
                      famsupsl + hisei + homepos + ictsch + icthome + ictqual + 
                      ictout + ictwkdy + icteffic + ictenq + ictfeed + cntschid")

# Run the regression model
lm_us <- lm.sdf(formula, data = pisa_dat)
summary(lm_us)

# checking VIF and assumptions
mod <- lm(pv1math ~ st004d01t + age + immig_recode + workpay_recode + misssc + 
            bsmj + relatst + learres +
            famsupsl + hisei + homepos + ictsch + icthome + ictqual + 
            ictout + ictwkdy + icteffic + ictenq + ictfeed + cntschid, data = pisa_dat)

car::vif(mod)
performance::check_model(mod)
plot(mod, 1)
plot(mod, 2)
plot(mod, 3)
plot(mod, 4)
plot(mod, 5)
plot(mod, 6)
car::ncvTest(mod)


### RQ1
lm_us <- lm.sdf(formula, data = pisa_dat)
summary(lm_us)
summary(lm_us, verbose = TRUE)


##### RQ2
library(EdSurvey)

# Null model (to get ICC)
model_null <- lmer.sdf(pvvars = "math",
                       formula = ~ 1 + (1 | cntschid),
                       data = pisa_dat)
summary(model_null)

# Model for RQ2: add Level-1 and Level-2 predictors
model_rq2 <- lmer.sdf(pvvars = "math",
                      formula = ~ st004d01t + age + immig_recode + workpay_recode + misssc +
                        bsmj + relatst + learres + famsupsl + hisei + homepos +
                        ictout + ictwkdy + icteffic + ictenq + ictfeed +
                        ictsch + ictqual + (1 | cntschid),
                      data = pisa_dat)
summary(model_rq2)



# Interaction effects

formula <- as.formula("math ~ st004d01t + age + immig + bsmj + relatst + learres +
                      famsupsl + hisei + homepos + ictsch + icthome + ictqual + 
                      ictout + ictwkdy + icteffic + ictenq + ictfeed + 
                      icteffic:bsmj + ictqual:bsmj + ictsch:relatst + 
                      ictwkdy:famsupsl + icthome:famsupsl + as.factor(cntschid)")

lm_us_int <- lm.sdf(formula, data = pisa_dat)
summary(lm_us_int)

formula <- as.formula("read ~ st004d01t + age + immig + bsmj + relatst + learres +
                      famsupsl + hisei + homepos + ictsch + icthome + ictqual + 
                      ictout + ictwkdy + icteffic + ictenq + ictfeed + 
                      icteffic:bsmj + ictqual:bsmj + ictsch:relatst + 
                      ictwkdy:famsupsl + icthome:famsupsl + as.factor(cntschid)")

lm_us_int <- lm.sdf(formula, data = pisa_dat)
summary(lm_us_int)




formula <- as.formula("scie ~ st004d01t + age + immig + bsmj + relatst + learres +
                      famsupsl + hisei + homepos + ictsch + icthome + ictqual + 
                      ictout + ictwkdy + icteffic + ictenq + ictfeed + 
                      icteffic:bsmj + ictqual:bsmj + ictsch:relatst + 
                      ictwkdy:famsupsl + icthome:famsupsl")

lm_us_int <- lm.sdf(formula, data = pisa_dat)
summary(lm_us_int)





# Define your formula including a random intercept for schools (cntschid)
formula <- as.formula("pv1math ~ st004d01t + age + immig + bsmj + relatst + learres +
                      famsupsl + hisei + homepos + ictsch + icthome + ictqual + 
                      ictout + ictwkdy + icteffic + ictenq + ictfeed + (1 | cntschid)")

# Run the multilevel model
mixed_model <- mixed.sdf(formula, data = pisa_dat, weightVar = c("w_fstuwt", "w_schgrnrabwt"))
summary(mixed_model)





# lm1 <- lm.sdf(math ~ st004d01t + age  + immig + bsmj  + relatst + learres
#             + famsupsl + hisced + hisei + homepos + ictsch + icthome + ictqual + 
#             ictout + ictwkdy + icteffic + ictenq + ictfeed + as.factor(cntschid), data = pisa_combined)
# summary(lm1)

lm2 <- lm.sdf(math ~ st004d01t + age  + immig + bsmj  + relatst + learres
              + famsupsl + hisei + homepos + ictsch + icthome + ictqual + 
                ictout + ictwkdy + icteffic + ictenq + ictfeed + cnt, data = pisa_combined)
summary(lm2)

lm_mod <- lm(pv1math ~ st004d01t + age  + immig + bsmj  + relatst + learres
+ famsupsl + hisced + hisei + homepos + ictsch + icthome + ictqual + 
  ictout + ictwkdy + icteffic + ictenq + ictfeed + cnt, data = pisa_combined)

lm_oecd <- lm.sdf(math ~ st004d01t + age  + immig + bsmj  + relatst + learres
              + famsupsl +  hisei + homepos + ictsch + icthome + ictqual*oecd + 
                ictout*oecd + ictwkdy*oecd + icteffic*oecd + ictenq*oecd + ictfeed*oecd, data = pisa_combined)
summary(lm_oecd)

lm_mod_int <- lm(pv1math ~ st004d01t + age  + immig + bsmj  + relatst + learres
             + famsupsl + hisei + homepos + ictsch + icthome + ictqual + 
               ictout + ictwkdy + icteffic + ictenq + ictfeed + oecd + 
               icteffic:bsmj + ictqual:bsmj + ictsch:relatst + 
               ictwkdy:famsupsl + icthome:famsupsl, weights = w_fstuwt, data = pisa_combined)
summary(lm_mod_int)

lm_oecd_int <- lm.sdf(math ~ st004d01t + age  + immig + bsmj  + relatst + learres
                  + famsupsl +  hisei + homepos + ictsch + icthome + ictqual + 
                    ictout + ictwkdy + icteffic + ictenq + ictfeed + oecd + 
                    icteffic:bsmj + ictqual:bsmj + ictsch:relatst + 
                    ictwkdy:famsupsl + icthome:famsupsl, data = pisa_combined, standardizeWithSamplingVar = TRUE)
summary(lm_oecd_int)


lm_int <- lm.sdf(math ~ st004d01t + age  + immig + bsmj  + relatst + learres
                      + famsupsl +  hisei + homepos + ictsch + icthome + ictqual + 
                        ictout + ictwkdy + icteffic + ictenq + ictfeed + 
                        icteffic:bsmj + ictqual:bsmj + ictsch:relatst + 
                        ictwkdy:famsupsl + icthome:famsupsl + as.factor(cntschid), data = pisa_dat_usa)
summary(lm_int)

table(pisa_combined$w_fstuwt)


lm_oecd_int <- lm.sdf(scie ~ st004d01t + age  + immig + bsmj  + relatst + learres
                      + famsupsl +  hisei + homepos + ictsch + icthome + ictqual + 
                        ictout + ictwkdy + icteffic + ictenq + ictfeed + oecd + icthome:famsupsl
                         , data = pisa_combined)
summary(lm_oecd_int)

icteffic:bsmj + ictqual:bsmj + ictsch:relatst + ictwkdy:famsupsl + icthome:famsupsl

lm_oecd_int <- lm.sdf(read ~ st004d01t + age  + immig + bsmj  + relatst + learres
                      + famsupsl +  hisei + homepos + ictsch + icthome + ictqual + 
                        ictout + ictwkdy + icteffic + ictenq + ictfeed + oecd + 
                        icteffic:bsmj + ictqual:bsmj + ictsch:relatst + 
                        ictwkdy:famsupsl + icthome:famsupsl, data = pisa_combined)
summary(lm_oecd_int)



lm_cnt_int <- lm.sdf(math ~ st004d01t + age  + immig + bsmj  + relatst + learres
                      + famsupsl +  hisei + homepos + ictsch + icthome + ictqual + 
                        ictout + ictwkdy + icteffic + ictenq + ictfeed + cnt + 
                        icteffic:bsmj + ictqual:bsmj + ictsch:relatst + 
                        ictwkdy:famsupsl + icthome:famsupsl, data = pisa_combined)
summary(lm_cnt_int)


pisa_combined$w_fschwt <- 1


mod <- mixed.sdf(math ~ st004d01t + oecd + age  + immig + bsmj  + relatst + learres
                 + famsupsl + hisei + homepos + ictsch + icthome + ictqual + 
                   ictout + ictwkdy + icteffic + ictenq + ictfeed+icteffic:bsmj + ictqual:bsmj + ictsch:relatst + ictwkdy:famsupsl + icthome:famsupsl+ (1|cnt), 
                 data = pisa_combined)
summary(mod)


mod <- mixed.sdf(math ~ st004d01t + oecd + age  + immig + bsmj  + relatst + learres
              + famsupsl + hisced + hisei + homepos + ictsch + icthome + ictqual + 
                ictout + ictwkdy + icteffic + ictenq + ictfeed+ (1|cnt), 
              data = pisa_combined, centerGroup = c(list("cnt" = ~ age  + bsmj  + 
                      relatst + learres + famsupsl + homepos + ictsch + icthome + ictqual
              + ictout + ictwkdy + icteffic + ictenq + ictfeed)))
summary(mod)

pisa_combined$w_fcntwt <- 1
pisa_combined$cntschid <- as.factor(pisa_combined$cntschid)

mod2 <- mixed.sdf(math ~ st004d01t + oecd + age  + immig + bsmj  + relatst + learres
                 + famsupsl + hisced + hisei + homepos + ictsch + icthome + ictqual + 
                   ictout + ictwkdy + icteffic + ictenq + ictfeed+ (1|cnt/cntschid) + (1|cnt), data = pisa_combined, weightVars = c("w_fstuwt", "w_fschwt", "w_fcntwt"))
summary(mod2)

janitor::tabyl(pisa_combined$cnt)

# view PV and weights
showPlausibleValues(data = pisa, verbose = TRUE)
showWeights(data = pisa, verbose = TRUE)

# summary/descriptive statistics for variables used in the analysis 
summary2(data = pisa, variable = )

# extract variables of interest to a dataframe using the getData funciton
dat <- getData(pisa, c(""), addAttributes=TRUE)


model_math <- lmer(PV1MATH ~ ICTSCH + ICTHOME + ICTQUAL + ICTOUT + ICTWKDY + ICTEFFIC + ICTENQ + ICTFEED +
                     BSMJ + RELATST + LEARRES + FAMSUPSL + HISCED + HISEI + HOMEPOS +
                     (1 | CNT) + (1 | CNTSCHID), 
                   data = pisa_subset, weights = W_FSTUWT)





