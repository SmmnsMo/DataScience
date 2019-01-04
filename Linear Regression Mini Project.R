## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to

##   1. Examine/plot the data before fitting the model

states.data <- readRDS("dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#Examining the first 10 rows of the data
head(states.info, 10)

##   2. Print and interpret the model `summary'
states.energy <- subset(states.data, select= c(metro, energy))
summary(states.energy)
summary(lm(energy ~ metro, data = states.data))

states.energy.model <- lm(energy ~ metro, data = states.data)
plot(states.energy.model)
par(mar = c(4, 4, 2, 2), mfrow = c(1, 2))
plot(states.energy.model, which = c(1, 2))
##   3. `plot' the model to look for deviations from modeling assumptions
plot(states.energy.model)

states.energy.model <- lm(energy ~ metro, data = states.data)
plot(states.energy.model)

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2))
plot(states.energy.model, which = c(1, 2))

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?

## Expanding our search to 20 variables
head(states.info, 20)

## Adding area to our model because its proportional to energy consumption
states.energy2 <- subset(states.data, select= c(energy, metro, area))
summary(states.energy2)

states.energy.model2 <- lm(energy~metro + area, data = na.omit(states.data))
summary(lm(energy~metro + area, data = states.data))

# We see that our area is significant and will incorporate that into our model.

## Plotting the model to look for deviations from modeling assumptions

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2))
plot(states.energy.model2, which = c(1, 2))

confint(states.energy.model2)
hist(residuals(states.energy.model2)) 

#The model is roughly normally distributed

## Exercise: interactions and factors
## ────────────────────────────────────────

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

# Adding interaction to the model
states.energy.int <- lm(energy ~metro*area, data = na.omit(states.data))
coef(summary(states.energy.int))

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

#Adding Region to our model
states.energy3 <- subset(states.data, select= c(energy, metro, area, region))
summary(states.energy3)

states.energy.model3 <- lm(energy~metro + area + region, data = na.omit(states.data))
summary(lm(energy~metro + area + region, data = states.data))


#Adding interaction term to our model (region)
states.energy.int2 <- lm(energy ~metro*area*region, data = na.omit(states.data))
coef(summary(states.energy.int2))
anova(states.energy.int2)

contrasts(states.data$region)
# change the reference group
coef(summary(lm(energy ~ C(region, base=4),
                data=states.data)))

# We find that there are no significant differences across the four regions.