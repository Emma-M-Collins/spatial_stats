# Emma Collins
# Math 6384 
# Spatial Project
# Fall 2019

# packages for spatial analysis
library("smerc") 

# packages for glm
library("car")
library("StepReg")
library("leaps")
library("bestglm")

# packages for plots
library("ggplot2")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("maps")
library("rgeos")
library("lwgeom")
library("geosphere")
library("RColorBrewer")

set.seed(23)

### Initial data ###
sdata <- read.csv("C:\\Users\\Owner\\Documents\\CU Denver Fall 2019\\Math 6384\\Project\\COCountyData.csv")
name <- sdata$COUNTY

### Plot setup ###
display.brewer.all(type = "all", colorblindFriendly = TRUE)
mygrad = brewer.pal(9, "PuRd")
mygrad1 = brewer.pal(9, "PuBuGn")
mygrad2 = brewer.pal(9, "PuBu")
mycol = brewer.pal(8, "Dark2")
mygrad3 <- brewer.pal(9, "YlOrRd")

world <- ne_countries(scale = "medium", returnclass = "sf")

states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))

counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("colorado,", counties$ID))
counties <- cbind(counties, name, centroid(as_Spatial(counties$geometry)))

### Initial Plot of Suicide Count/Proportion ###
#Suicide Count 2010-2018
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5), aes(fill = as.numeric(sdata$S1018))) + 
  # geom_point(data = coords, mapping = aes(x = counties.X1, y = counties.X2)) + remove for centroids
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  scale_fill_continuous(low = mygrad3[1], high = mygrad3[9], name = "Count") +
  labs(title = "Suicide Count by County, 2010-2018", x = "", y = "")

#Suicide Proportion 2010-2018
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5), aes(fill = (as.numeric(sdata$S1018)/as.numeric(sdata$Tpop)))) + 
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  scale_fill_continuous(low = mygrad3[1], high = mygrad3[9], name = "Proportion") +
  ggtitle("Proportion of Suicide by County, 2010-2018")



########## CLUSTERING TESTS ############

# look at summary statistics to identify potential limits (c*, n*)
summary(sdata)

#S1018, 1st Q: 10
#       Mean: 146
#       Median: 33
#       3rd Q: 85

# Population 1st Q: 5734
#            Mean: 78581
#            Median: 15084
#            3rd Q: 42663


# Prep for cluster tests
coords <- data.frame(counties$X1, counties$X2)
pop <- as.numeric(sdata$Tpop)
cases <- as.numeric(sdata$S1018)

# Besag-Newell
# addresses variability in local incidence proption by fixing lower bound for number of 
#   cases in each window by c*.  
# H0: Most compact window w/ c* cases (centered at centroid) is not sig. more compact than what
#   is expected under CRH

bnl <- bn.test(coords = coords, cases = cases, pop = pop, cstar = 20, alpha = 0.05, longlat = TRUE)
bnl 
bnl_c <- counties[sort(bnl$clusters[[1]]$locids),]
bnl_c2 <- counties[sort(bnl$clusters[[2]]$locids),]
#bnl_c3 <- counties[sort(bnl$clusters[[3]]$locids),]

#BN, c* = 20, results
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
 #geom_sf(data = bnl_c3, fill = mygrad[8]) +
  geom_sf(data = bnl_c2, fill = mygrad[6]) +
  geom_sf(data = bnl_c, fill = mygrad[4]) +
  geom_text(data = bnl_c, aes(x=X1, y=X2, label=bnl_c$name)) +
  geom_text(data = bnl_c2, aes(x=X1, y=X2, label=bnl_c2$name)) + 
 #geom_text(data = bnl_c3, aes(x=X1, y=X2, label=bnl_c3$name)) + 
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "Besag-Newell Test Results, c*=20", x = "", y = "") 

bnm <- bn.test(coords = coords, cases = cases, pop = pop, cstar = 50, alpha = 0.05, longlat = TRUE)
bnm
bnm_c <- counties[sort(bnm$clusters[[1]]$locids),]
bnm_c2 <- counties[sort(bnm$clusters[[2]]$locids),]
#bnm_c3 <- counties[sort(bnm$clusters[[3]]$locids),]

#BN, c* = 50, results
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
 # geom_sf(data = bnm_c3, fill = mygrad[8]) +
  geom_sf(data = bnm_c2, fill = mygrad[6]) +
  geom_sf(data = bnm_c, fill = mygrad[4]) +
  geom_text(data = bnm_c, aes(x=X1, y=X2, label=bnm_c$name)) +
  geom_text(data = bnm_c2, aes(x=X1, y=X2, label=bnm_c2$name)) + 
  #geom_text(data = bnm_c3, aes(x=X1, y=X2, label=bnm_c3$name)) + 
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "Besag-Newell Test Results, c*=50", x = "", y = "") 

bnh <- bn.test(coords = coords, cases = cases, pop = pop, cstar = 100, alpha = 0.05, longlat = TRUE)
bnh 
bnh_c <- counties[sort(bnh$clusters[[1]]$locids),]
bnh_c2 <- counties[sort(bnh$clusters[[2]]$locids),]
bnh_c3 <- counties[sort(bnh$clusters[[3]]$locids),]

#BN, c* = 100, results
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  geom_sf(data = bnh_c3, fill = mygrad[8]) +
  geom_sf(data = bnh_c2, fill = mygrad[6]) +
  geom_sf(data = bnh_c, fill = mygrad[4]) +
  geom_text(data = bnh_c, aes(x=X1, y=X2, label=bnh_c$name)) +
  geom_text(data = bnh_c2, aes(x=X1, y=X2, label=bnh_c2$name)) + 
  geom_text(data = bnh_c3, aes(x=X1, y=X2, label=bnh_c3$name)) + 
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "Besag-Newell Test Results, c*=100", x = "", y = "") 

# Plot of most likely cluster from each BN test
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  geom_sf(data = bnh_c, fill = mygrad[8]) +
  geom_sf(data = bnm_c, fill = mygrad[7]) +
  geom_sf(data = bnl_c, fill = mygrad[6]) +
  geom_text(data = bnl_c, aes(x=X1, y=X2, label=bnl_c$name)) +
  geom_text(data = bnm_c, aes(x=X1, y=X2, label=bnm_c$name)) + 
  geom_text(data = bnh_c, aes(x=X1, y=X2, label=bnh_c$name)) + 
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "Besag-Newell Test Results", x = "", y = "") 

# CEPP
# Chose fixed persons at risk, n*, expand window until n* is reached
# H0: No window with n* persons at risk has sig. more cases than what we expect under CRH 

ceppl <- cepp.test(coords = coords, cases = cases, pop = pop, nstar = 20000, alpha = 0.05, longlat = TRUE)
ceppl 
ceppl_c <- counties[sort(ceppl$clusters[[1]]$locids),]
ceppl_c2 <- counties[sort(ceppl$clusters[[2]]$locids),]
ceppl_c3 <- counties[sort(ceppl$clusters[[3]]$locids),]

#CEPP, n* = 20000, results
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  geom_sf(data = ceppl_c3, fill = mygrad1[8]) +
  geom_sf(data = ceppl_c2, fill = mygrad1[6]) +
  geom_sf(data = ceppl_c, fill = mygrad1[4]) +
  geom_text(data = ceppl_c3, aes(x=X1, y=X2, label=ceppl_c3$name)) +
  geom_text(data = ceppl_c2, aes(x=X1, y=X2, label=ceppl_c2$name)) + 
  geom_text(data = ceppl_c, aes(x=X1, y=X2, label=ceppl_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "CEPP Test Results, n*=20,000", x = "", y = "") 

ceppm <- cepp.test(coords = coords, cases = cases, pop = pop, nstar = 100000, alpha = 0.05, longlat = TRUE)
ceppm 
ceppm_c <- counties[sort(ceppm$clusters[[1]]$locids),]
ceppm_c2 <- counties[sort(ceppm$clusters[[2]]$locids),]
ceppm_c3 <- counties[sort(ceppm$clusters[[3]]$locids),]

#CEPP, n* = 100000, results
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  geom_sf(data = ceppm_c3, fill = mygrad1[8]) +
  geom_sf(data = ceppm_c2, fill = mygrad1[6]) +
  geom_sf(data = ceppm_c, fill = mygrad1[4]) +
  geom_text(data = ceppm_c3, aes(x=X1, y=X2, label=ceppm_c3$name)) +
  geom_text(data = ceppm_c2, aes(x=X1, y=X2, label=ceppm_c2$name)) + 
  geom_text(data = ceppm_c, aes(x=X1, y=X2, label=ceppm_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "CEPP Test Results, n*=100,000", x = "", y = "") 

cepph <- cepp.test(coords = coords, cases = cases, pop = pop, nstar = 200000, alpha = 0.05, longlat = TRUE)
cepph 
cepph_c <- counties[sort(cepph$clusters[[1]]$locids),]
cepph_c2 <- counties[sort(cepph$clusters[[2]]$locids),]
#cepph_c3 <- counties[sort(cepph$clusters[[3]]$locids),]

#CEPP, n* = 200000, results
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  #geom_sf(data = cepph_c3, fill = mygrad1[8]) +
  geom_sf(data = cepph_c2, fill = mygrad1[6]) +
  geom_sf(data = cepph_c, fill = mygrad1[4]) +
 # geom_text(data = cepph_c3, aes(x=X1, y=X2, label=cepph_c3$name)) +
  geom_text(data = cepph_c2, aes(x=X1, y=X2, label=cepph_c2$name)) + 
  geom_text(data = cepph_c, aes(x=X1, y=X2, label=cepph_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "CEPP Test Results, n* = 200,000", x = "", y = "") 

# Most likely cluster from each CEPP Test
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  geom_sf(data = cepph_c, fill = mygrad1[8]) +
  geom_sf(data = ceppm_c, fill = mygrad1[7]) +
  geom_sf(data = ceppl_c, fill = mygrad1[6]) +
  geom_text(data = ceppl_c, aes(x=X1, y=X2, label=ceppl_c$name)) +
  geom_text(data = ceppm_c, aes(x=X1, y=X2, label=ceppm_c$name)) + 
  geom_text(data = cepph_c, aes(x=X1, y=X2, label=cepph_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "CEPP Test Results", x = "", y = "") 


# Spatial Scan Statistics
# Chooses population upper bound, identifies cluster(s) least compatible with CRH
# H0: the most likely cluster is consistent with what is expected under CRH

ssl <- scan.test(coords = coords, cases = cases, pop = pop, ubpop = 0.05, alpha = 0.05, longlat = TRUE)
ssl 
ssl_c <- counties[sort(ssl$clusters[[1]]$locids),]
ssl_c2 <- counties[sort(ssl$clusters[[2]]$locids),]
ssl_c3 <- counties[sort(ssl$clusters[[3]]$locids),]

#SS, up=0.05
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  geom_sf(data = ssl_c3, fill = mygrad2[8]) +
  geom_sf(data = ssl_c2, fill = mygrad2[6]) +
  geom_sf(data = ssl_c, fill = mygrad2[4]) +
  geom_text(data = ssl_c3, aes(x=X1, y=X2, label=ssl_c3$name)) +
  geom_text(data = ssl_c2, aes(x=X1, y=X2, label=ssl_c2$name)) +
  geom_text(data = ssl_c, aes(x=X1, y=X2, label=ssl_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "Spatial Scan Test Results, upper bound = 0.05", x = "", y = "") 

ssm <- scan.test(coords = coords, cases = cases, pop = pop, ubpop = 0.15, alpha = 0.05, longlat = TRUE)
ssm 
ssm_c <- counties[sort(ssm$clusters[[1]]$locids),]
ssm_c2 <- counties[sort(ssm$clusters[[2]]$locids),]
#ssm_c3 <- counties[sort(ssm$clusters[[3]]$locids),]

#SS, up=0.15
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
 # geom_sf(data = ssm_c3, fill = mygrad2[8]) +
  geom_sf(data = ssm_c2, fill = mygrad2[6]) +
  geom_sf(data = ssm_c, fill = mygrad2[4]) +
  #geom_text(data = ssm_c3, aes(x=X1, y=X2, label=ssm_c3$name)) +
  geom_text(data = ssm_c2, aes(x=X1, y=X2, label=ssm_c2$name)) +
  geom_text(data = ssm_c, aes(x=X1, y=X2, label=ssm_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "Spatial Scan Test Results, upper bound = 0.15", x = "", y = "") 

ssh <- scan.test(coords = coords, cases = cases, pop = pop, ubpop = 0.25, alpha = 0.05, longlat = TRUE)
ssh 
ssh_c <- counties[sort(ssh$clusters[[1]]$locids),]
ssh_c2 <- counties[sort(ssh$clusters[[2]]$locids),]
#ssh_c3 <- counties[sort(ssh$clusters[[3]]$locids),]

#SS, up=0.25
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  #geom_sf(data = ssh_c3, fill = mygrad2[8]) +
  geom_sf(data = ssh_c2, fill = mygrad2[6]) +
  geom_sf(data = ssh_c, fill = mygrad2[4]) +
 # geom_text(data = ssh_c3, aes(x=X1, y=X2, label=ssh_c3$name)) +
  geom_text(data = ssh_c2, aes(x=X1, y=X2, label=ssh_c2$name)) +
  geom_text(data = ssh_c, aes(x=X1, y=X2, label=ssh_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "Spatial Scan Test Results, upper bound = 0.25", x = "", y = "") 

# most likely cluster from each test level
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  geom_sf(data = ssh_c, fill = mygrad2[8]) +
  geom_sf(data = ssm_c, fill = mygrad2[7]) +
  geom_sf(data = ssl_c, fill = mygrad2[6]) +
  geom_text(data = ssl_c, aes(x=X1, y=X2, label=ssl_c$name)) +
  geom_text(data = ssm_c, aes(x=X1, y=X2, label=ssm_c$name)) +
  geom_text(data = ssh_c, aes(x=X1, y=X2, label=ssh_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "Spatial Scan Test Results", x = "", y = "") 

require(gridExtra)
ceppr <- ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  geom_sf(data = cepph_c, fill = mygrad1[8]) +
  geom_sf(data = ceppm_c, fill = mygrad1[7]) +
  geom_sf(data = ceppl_c, fill = mygrad1[6]) +
  geom_text(data = ceppl_c, aes(x=X1, y=X2, label=ceppl_c$name)) +
  geom_text(data = ceppm_c, aes(x=X1, y=X2, label=ceppm_c$name)) + 
  geom_text(data = cepph_c, aes(x=X1, y=X2, label=cepph_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "CEPP Test Results", x = "", y = "")
sscan <- ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  geom_sf(data = ssh_c, fill = mygrad2[8]) +
  geom_sf(data = ssm_c, fill = mygrad2[7]) +
  geom_sf(data = ssl_c, fill = mygrad2[6]) +
  geom_text(data = ssl_c, aes(x=X1, y=X2, label=ssl_c$name)) +
  geom_text(data = ssm_c, aes(x=X1, y=X2, label=ssm_c$name)) +
  geom_text(data = ssh_c, aes(x=X1, y=X2, label=ssh_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "Spatial Scan Test Results", x = "", y = "") 
grid.arrange(ceppr, sscan, ncol=2)


########### BUILDING GLM #############

### collinearity check

# pairwise correlation (first column is county name)

cor(sdata[,2:11])
# obvious correlation between population and the two different suicide counts.  Exclude Suicide 10

cor(sdata[, c(2, 4:11)])
# high correlation between suicide counts and population, remove population

cor(sdata[, 4:11])
# high-ish correlation between HS and Bachelors, but we'll leave it in for now

# VIF
vifmod <- glm(S1018 ~ IPC + Rural + HS + Bachelors + Poverty + Female + HomeOwn, 
              data = sdata, family = "poisson", offset = log(Tpop))
vif(vifmod)
# Bachelors and HS again are pretty correlated.  Remove HS

vifmod2 <- update(vifmod, .~.-HS)
vif(vifmod2)
# IPC is pretty high, let's see what happens if we take the full model and just remove IPC

vifmod3 <- update(vifmod, .~.-IPC)
vif(vifmod3)
# Bachelors and HS stay low.  Let's just try removing Bachelors just to see if that makes any change

vifmod4 <- update(vifmod, .~.-Bachelors)
vif(vifmod4)
# All other VIF's are acceptable.  Let's remove bachelors, keep in IPC and HS

### Model Selection

# We'll use AIC stepwise selection, BIC stepwise selection

# fit full model first.

fullmod <- glm(S1018 ~ IPC + Rural + HS + Poverty + Female + HomeOwn, data = sdata, 
               family = "poisson", offset = log(Tpop))

AICmod <- step(fullmod, direction = "both", trace = FALSE)
summary(AICmod)
# model chosen by AIC is all predictors except female

BICmod <- step(fullmod, direction = "both", trace = FALSE, k = log(64))
summary(BICmod)
# model chosen by BIC is also all predictors except female

### Final model

finalmod <- BICmod
summary(finalmod)

### Model assumptions

# normality
qqnorm(residuals(finalmod), pch = 16)
qqline(residuals(finalmod))
#doesn't look great, but it's not awful bad

# homogeneous variance
plot(fitted(finalmod), residuals(finalmod), main="Resid V Fitted")
abline(h=0)
# The lower values are a little concerning with a slight downward pattern...

plot(fitted(finalmod), sqrt(abs(residuals(finalmod))), main="Sqrt(Resid) V Fitted")

# outliers/infulential obsv
outlierTest(finalmod)
influencePlot(finalmod)
# potential observations, let's see what happens when they're removed

block <- row.names(sdata)

# remove 17
mod1 <- glm(S1018 ~ IPC + Rural + HS + Poverty + HomeOwn, data = sdata, family = "poisson", offset = log(Tpop),
            subset = (block != 17))
compareCoefs(finalmod, mod1)
# some fair change in coefficients, but nothing awful (no sign changes)

# remove 19
mod2 <- glm(S1018 ~ IPC + Rural + HS + Poverty + HomeOwn, data = sdata, family = "poisson", offset = log(Tpop),
            subset = (block != 19))
compareCoefs(finalmod, mod2)

# remove 21
mod3 <- glm(S1018 ~ IPC + Rural + HS + Poverty + HomeOwn, data = sdata, family = "poisson", offset = log(Tpop),
            subset = (block != 21))
compareCoefs(finalmod, mod3)

# remove 40
mod4 <- glm(S1018 ~ IPC + Rural + HS + Poverty + HomeOwn, data = sdata, family = "poisson", offset = log(Tpop),
            subset = (block != 40))
compareCoefs(finalmod, mod4)

# remove 50
mod5 <- glm(S1018 ~ IPC + Rural + HS + Poverty + HomeOwn, data = sdata, family = "poisson", offset = log(Tpop),
            subset = (block != 50))
compareCoefs(finalmod, mod5)

# nothing warrents removal.

# Will use for expected number of cases for tests

glm_pred <- predict.glm(finalmod, type = "response")

############ POST-GLM CLUSTERING TESTS ##############

# CEPP
# Chose fixed persons at risk, n*, expand window until n* is reached
# H0: No window with n* persons at risk has sig. more cases than what we expect under CRH 

cepplp <- cepp.test(coords = coords, cases = cases, pop = pop, ex = glm_pred, nstar = 20000, alpha = 0.05, longlat = TRUE)
cepplp 
cepplp_c <- counties[sort(cepplp$clusters[[1]]$locids),]
cepplp_c2 <- counties[sort(cepplp$clusters[[2]]$locids),]
#cepplp_c3 <- counties[sort(cepplp$clusters[[3]]$locids),]

# CEPP, GLM Expected Counts, n*=20000
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  #geom_sf(data = cepplp_c3, fill = mygrad1[8]) +
  geom_sf(data = cepplp_c2, fill = mygrad1[6]) +
  geom_sf(data = cepplp_c, fill = mygrad1[4]) +
  #geom_text(data = cepplp_c3, aes(x=X1, y=X2, label=cepplp_c3$name)) +
  geom_text(data = cepplp_c2, aes(x=X1, y=X2, label=cepplp_c2$name)) + 
  geom_text(data = cepplp_c, aes(x=X1, y=X2, label=cepplp_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "CEPP Test Results with Poisson GLM Expected Counts, n*=20,000", x = "", y = "")


ceppmp <- cepp.test(coords = coords, cases = cases, pop = pop, ex = glm_pred, nstar = 100000, alpha = 0.05, longlat = TRUE)
ceppmp 
ceppmp_c <- counties[sort(ceppmp$clusters[[1]]$locids),]
#ceppmp_c2 <- counties[sort(ceppmp$clusters[[2]]$locids),]
#ceppmp_c3 <- counties[sort(ceppmp$clusters[[3]]$locids),]

# CEPP, GLM Expected Counts, n*=100000
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  #geom_sf(data = ceppmp_c3, fill = mygrad1[8]) +
  #geom_sf(data = ceppmp_c2, fill = mygrad1[6]) +
  geom_sf(data = ceppmp_c, fill = mygrad1[4]) +
  #geom_text(data = ceppmp_c3, aes(x=X1, y=X2, label=ceppmp_c3$name)) +
  #geom_text(data = ceppmp_c2, aes(x=X1, y=X2, label=ceppmp_c2$name)) + 
  geom_text(data = ceppmp_c, aes(x=X1, y=X2, label=ceppmp_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "CEPP Test Results with Poisson GLM Expected Counts, n*=100,000", x = "", y = "")


cepphp <- cepp.test(coords = coords, cases = cases, pop = pop, ex = glm_pred, nstar = 200000, alpha = 0.05, longlat = TRUE)
cepphp 
cepphp_c <- counties[sort(cepphp$clusters[[1]]$locids),]
#cepphp_c2 <- counties[sort(cepphp$clusters[[2]]$locids),]
#cepphp_c3 <- counties[sort(cepphp$clusters[[3]]$locids),]

# CEPP, GLM Expected Counts, n*=100000
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  #geom_sf(data = ceppmp_c3, fill = mygrad1[8]) +
  #geom_sf(data = ceppmp_c2, fill = mygrad1[6]) +
  geom_sf(data = cepphp_c, fill = mygrad1[4]) +
  #geom_text(data = ceppmp_c3, aes(x=X1, y=X2, label=ceppmp_c3$name)) +
  #geom_text(data = ceppmp_c2, aes(x=X1, y=X2, label=ceppmp_c2$name)) + 
  geom_text(data = cepphp_c, aes(x=X1, y=X2, label=cepphp_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "CEPP Test Results with Poisson GLM Expected Counts, n*=200,000", x = "", y = "")


#Top clusters from each test level
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  geom_sf(data = cepphp_c, fill = mygrad1[8]) +
  geom_sf(data = cepplp_c, fill = mygrad1[6]) +
  geom_sf(data = ceppmp_c, fill = mygrad1[7]) +
  geom_text(data = cepplp_c, aes(x=X1, y=X2, label=cepplp_c$name)) +
  geom_text(data = ceppmp_c, aes(x=X1, y=X2, label=ceppmp_c$name)) + 
  geom_text(data = cepphp_c, aes(x=X1, y=X2, label=cepphp_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "CEPP Test Results with Poisson GLM Expected Counts", x = "", y = "") 

# Spatial Scan Statistics
# Chooses population upper bound, identifies cluster(s) least compatible with CRH
# H0: the most likely cluster is consistent with what is expected under CRH

sslp <- scan.test(coords = coords, cases = cases, pop = pop, ex = glm_pred, ubpop = 0.05, alpha = 0.05, longlat = TRUE)
sslp 
sslp_c <- counties[sort(sslp$clusters[[1]]$locids),]
sslp_c2 <- counties[sort(sslp$clusters[[2]]$locids),]
#sslp_c3 <- counties[sort(sslp$clusters[[3]]$locids),]

# SS test with GLM counts, up = 0.05
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  #geom_sf(data = sslp_c3, fill = mygrad2[8]) +
  geom_sf(data = sslp_c2, fill = mygrad2[6]) +
  geom_sf(data = sslp_c, fill = mygrad2[4]) +
  #geom_text(data = sslp_c3, aes(x=X1, y=X2, label=sslp_c3$name)) +
  geom_text(data = sslp_c2, aes(x=X1, y=X2, label=sslp_c2$name)) +
  geom_text(data = sslp_c, aes(x=X1, y=X2, label=sslp_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "Spatial Scan Test Results with Poisson GLM Expected Counts, upper bound = 0.05", x = "", y = "") 

ssmp <- scan.test(coords = coords, cases = cases, pop = pop, ex = glm_pred, ubpop = 0.15, alpha = 0.05, longlat = TRUE)
ssmp 
ssmp_c <- counties[sort(ssmp$clusters[[1]]$locids),]
#ssmp_c2 <- counties[sort(ssmp$clusters[[2]]$locids),]
#ssmp_c3 <- counties[sort(ssmp$clusters[[3]]$locids),]

# SS test with GLM counts, up = 0.15
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  #geom_sf(data = sslp_c3, fill = mygrad2[8]) +
  #geom_sf(data = sslp_c2, fill = mygrad2[6]) +
  geom_sf(data = ssmp_c, fill = mygrad2[4]) +
  #geom_text(data = sslp_c3, aes(x=X1, y=X2, label=sslp_c3$name)) +
  #geom_text(data = sslp_c2, aes(x=X1, y=X2, label=sslp_c2$name)) +
  geom_text(data = ssmp_c, aes(x=X1, y=X2, label=ssmp_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "Spatial Scan Test Results with Poisson GLM Expected Counts, upper bound = 0.15", x = "", y = "") 

sshp <- scan.test(coords = coords, cases = cases, pop = pop, ex = glm_pred, ubpop = 0.25, alpha = 0.05, longlat = TRUE)
sshp 
sshp_c <- counties[sort(sshp$clusters[[1]]$locids),]
#sshp_c2 <- counties[sort(sshp$clusters[[2]]$locids),]
#sshp_c3 <- counties[sort(sshp$clusters[[3]]$locids),]

ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  #geom_sf(data = sslp_c3, fill = mygrad2[8]) +
  #geom_sf(data = sslp_c2, fill = mygrad2[6]) +
  geom_sf(data = sshp_c, fill = mygrad2[4]) +
  #geom_text(data = sslp_c3, aes(x=X1, y=X2, label=sslp_c3$name)) +
  #geom_text(data = sslp_c2, aes(x=X1, y=X2, label=sslp_c2$name)) +
  geom_text(data = sshp_c, aes(x=X1, y=X2, label=sshp_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "Spatial Scan Test Results with Poisson GLM Expected Counts, upper bound = 0.25", x = "", y = "") 

# top clusters from each Spatial Scan Test
ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  geom_sf(data = sshp_c, fill = mygrad2[8]) +
  geom_sf(data = ssmp_c, fill = mygrad2[7]) +
  geom_sf(data = sslp_c, fill = mygrad2[6]) +
  geom_text(data = sslp_c, aes(x=X1, y=X2, label=sslp_c$name)) +
  geom_text(data = ssmp_c, aes(x=X1, y=X2, label=ssmp_c$name)) +
  geom_text(data = sshp_c, aes(x=X1, y=X2, label=sshp_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "Spatial Scan Test Results with Poisson GLM Expected Counts", x = "", y = "") 


ceppglm <- ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  geom_sf(data = cepphp_c, fill = mygrad1[8]) +
  geom_sf(data = cepplp_c, fill = mygrad1[6]) +
  geom_sf(data = ceppmp_c, fill = mygrad1[7]) +
  geom_text(data = cepplp_c, aes(x=X1, y=X2, label=cepplp_c$name)) +
  geom_text(data = ceppmp_c, aes(x=X1, y=X2, label=ceppmp_c$name)) + 
  geom_text(data = cepphp_c, aes(x=X1, y=X2, label=cepphp_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "CEPP Test Results with Poisson GLM Expected Counts", x = "", y = "") 

  
sscanglm <- ggplot(data = world) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = counties, color = gray(.5)) + 
  geom_sf(data = sshp_c, fill = mygrad2[8]) +
  geom_sf(data = ssmp_c, fill = mygrad2[7]) +
  geom_sf(data = sslp_c, fill = mygrad2[6]) +
  geom_text(data = sslp_c, aes(x=X1, y=X2, label=sslp_c$name)) +
  geom_text(data = ssmp_c, aes(x=X1, y=X2, label=ssmp_c$name)) +
  geom_text(data = sshp_c, aes(x=X1, y=X2, label=sshp_c$name)) +
  coord_sf(xlim = c(-110, -101), ylim = c(36.5, 41.5), expand = FALSE) +
  labs(title = "Spatial Scan Test Results with Poisson GLM Expected Counts", x = "", y = "") 

grid.arrange(ceppglm, sscanglm, ncol=2)

#### What if? #####

# Rural areas tend to have higher rates of suicide.  What happens if we fit Pois GLM with rural only?

testglm <- glm(S1018 ~ Rural, data = sdata, family = "poisson", offset = log(Tpop))
rural_pred <- predict(testglm, type = "response")

cepplr <- cepp.test(coords = coords, cases = cases, pop = pop, ex = rural_pred, nstar = 20000, alpha = 0.05, longlat = TRUE)
cepplr
sort(cepplr$clusters[[1]]$locids)
sort(cepplp$clusters[[1]]$locids)

ceppmr <- cepp.test(coords = coords, cases = cases, pop = pop, ex = rural_pred, nstar = 100000, alpha = 0.05, longlat = TRUE)
ceppmr
sort(ceppmr$clusters[[1]]$locids)
sort(ceppmp$clusters[[1]]$locids)

cepphr <- cepp.test(coords = coords, cases = cases, pop = pop, ex = rural_pred, nstar = 200000, alpha = 0.05, longlat = TRUE)
cepphr
sort(cepphr$clusters[[1]]$locids)
sort(cepphp$clusters[[1]]$locids)

sslr <- scan.test(coords = coords, cases = cases, pop = pop, ex = rural_pred, ubpop = 0.05, alpha = 0.05, longlat = TRUE)
sslr 
sort(sslr$clusters[[1]]$locids)
sort(sslp$clusters[[1]]$locids)

ssmr <- scan.test(coords = coords, cases = cases, pop = pop, ex = rural_pred, ubpop = 0.25, alpha = 0.05, longlat = TRUE)
ssmr 
sort(ssmr$clusters[[1]]$locids)
sort(ssmp$clusters[[1]]$locids)

sshr <- scan.test(coords = coords, cases = cases, pop = pop, ex = rural_pred, ubpop = 0.25, alpha = 0.05, longlat = TRUE)
sshr 
sort(sshr$clusters[[1]]$locids)
sort(sshp$clusters[[1]]$locids)

# No significant changes when comparing most likely cluster.  CEPP tests are exactly the same, 
#    Spatial scan only slightly different - only includes more counties

### What about fitting suicide counts to just HS degrees?

testglm2 <-  glm(S1018 ~ HS, data = sdata, family = "poisson", offset = log(Tpop))
HS_pred <- predict(testglm2, type = "response")

cepplh <- cepp.test(coords = coords, cases = cases, pop = pop, ex = hs_pred, nstar = 20000, alpha = 0.05, longlat = TRUE)
cepplh
sort(cepplh$clusters[[1]]$locids)
sort(cepplp$clusters[[1]]$locids)

ceppmh <- cepp.test(coords = coords, cases = cases, pop = pop, ex = hs_pred, nstar = 100000, alpha = 0.05, longlat = TRUE)
ceppmh
sort(ceppmh$clusters[[1]]$locids)
sort(ceppmp$clusters[[1]]$locids)

cepphh <- cepp.test(coords = coords, cases = cases, pop = pop, ex = hs_pred, nstar = 200000, alpha = 0.05, longlat = TRUE)
cepphh
sort(cepphh$clusters[[1]]$locids)
sort(cepphp$clusters[[1]]$locids)

sslh <- scan.test(coords = coords, cases = cases, pop = pop, ex = hs_pred, ubpop = 0.05, alpha = 0.05, longlat = TRUE)
sslh 
sort(sslh$clusters[[1]]$locids)
sort(sslp$clusters[[1]]$locids)

ssmh <- scan.test(coords = coords, cases = cases, pop = pop, ex = hs_pred, ubpop = 0.25, alpha = 0.05, longlat = TRUE)
ssmh 
sort(ssmh$clusters[[1]]$locids)
sort(ssmp$clusters[[1]]$locids)

sshh <- scan.test(coords = coords, cases = cases, pop = pop, ex = hs_pred, ubpop = 0.25, alpha = 0.05, longlat = TRUE)
sshh 
sort(sshh$clusters[[1]]$locids)
sort(sshp$clusters[[1]]$locids)

# Again, no significantly changing results when comparing most likely cluster for each test
# CEPP the same, Spatial scan the same or includes previous counties and 1-3 less counties
