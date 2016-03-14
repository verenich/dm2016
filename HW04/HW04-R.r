
dat = read.csv("ncmp_1415_final_non_disclosive.csv",header=TRUE,sep=",")

set.seed(123)

head(dat)

dim(dat)

dat = dat[(dat$height > 0 & dat$weight > 0 & dat$bmi > 0),]

dim(dat)

hist(dat$ageinmonths / 12,breaks = 50)

dat$age = dat$ageinmonths / 12
dat$agegroup = as.factor(ifelse(dat$age >= 4 & dat$age <=6, "1", "2"))

dat = dat[,c("genderdescription","age","agegroup","height","weight","bmi","bmipopulationcategory")]

ids = sample(1:nrow(dat),round(0.03*nrow(dat)),replace = FALSE)

dat = dat[ids,]

dim(dat)

dat$age.x.gender = as.factor(mapply(paste0, dat$agegroup, dat$genderdescr))


dat$bmicat.agegroup = as.factor(mapply(paste0, dat$bmipopulationcategory, dat$agegroup))

library(ggplot2)

ggplot(dat) + geom_point(aes(x = age, y = weight,color = genderdescription)) +
facet_wrap(~agegroup, scales = "free_x",labeller = labeller(agegroup =  c('1'= "4-5 yrs", '2' = "10-11 yrs"))) +
scale_color_discrete("Gender") + xlab("Age (yrs)") + ylab("Weight (kg)") + 
theme(legend.justification = c(0, 1),legend.position = c(0,1))


ggplot(dat) + geom_point(aes(x = age, y = bmi, color = genderdescription)) +
facet_wrap( ~agegroup, scales = "free_x", labeller = labeller(agegroup = c('1'= "4-5 yrs", '2' = "10-11 yrs"))) +
scale_color_discrete("Gender") + xlab("Age (yrs)") + 
ylab("BMI") + theme(legend.justification = c(0, 1),legend.position = c(0,1))


ggplot(dat) + geom_point(aes(x = height, y = weight,color = genderdescription)) +
facet_wrap(~agegroup, scales = "free_x",labeller = labeller(agegroup = c('1'= "4-5 yrs", '2' = "10-11 yrs")))+
scale_color_discrete("Gender") + xlab("Height (cm)") + ylab("Weight (kg)") +
theme(legend.justification = c(0, 1),legend.position = c(0,1))

ggplot(subset(dat, agegroup == "1")) + geom_point(aes(x = age, y = bmi,color = bmipopulationcategory)) +
facet_wrap(~genderdescription, scales = "free_x") + scale_color_discrete("Category by BMI") +
xlab("Age (yrs)") + ylab("BMI") + theme(legend.position = "top") + ggtitle("BMI for the age of group 4-5 yrs")



ggplot(subset(dat, agegroup == "2")) + geom_point(aes(x = age, y = bmi,color = bmipopulationcategory)) +
facet_wrap(~genderdescription, scales = "free_x") + scale_color_discrete("Category by BMI") +
xlab("Age (yrs)") + ylab("BMI") + theme(legend.position = "top") + ggtitle("BMI for the age of group 10-11 yrs")

dat$height.x.weight = dat$height * dat$weight
dat$height.d.weight = dat$height / dat$weight
bsa = function(w, h) 0.007184 * w^0.425 * h^0.725
dat$bsa = mapply(bsa, dat$weight, dat$height)


ggplot(dat) + geom_point(aes(x = height, y = bsa, color = bmicat.agegroup)) +
scale_color_manual(values = c("#00AFB2", "#0072B2", "#6C9E00", "#009E44","#E69F00", "#E65800", "#D500B9", "#D50055"),
name = "BMI by category\n& age",labels = c("healthy weight (4-5 yrs)",
"healthy weight (10-11 yrs)",
"overweight (4-5 yrs)", "overweight (10-11 yrs)",
"underweight (4-5 yrs)", "underweight (10-11 yrs)",
"very overweight (4-5 yrs)",
"very overweight (10-11 yrs)")) +
theme(legend.position = "top") +
xlab("Height (cm)") +
ylab("BSA")


normalize <- function (x) {
x$normheight <- (x$height - mean(x$height)) / sd(x$height)
x$normweight <- (x$weight - mean(x$weight)) / sd(x$weight)
x
}

tmp <- by(dat, list(dat$genderdescription, dat$age), normalize)
normalized <- do.call('rbind', tmp)

NB = length(levels(normalized$bmipopulationcategory))
NG = length(levels(normalized$genderdescription))

plot(normalized$age, normalized$normheight, xlab='Age in months', ylab='Height score',
col = 1:NB,pch = 1:NG)
legend('center', legend = levels(normalized$bmipopulationcategory), col = 1:NB, lwd = 3)

plot(normalized$normheight, normalized$normweight, xlab='Height score', ylab='Weight score',
col = 1:NB, pch = 1:NG)
legend('topleft', legend = levels(normalized$bmipopulationcategory), col = 1:NB, lwd = 3)

library(reshape2)

dat_younger = subset(dat, agegroup == "1")
dat_older = subset(dat, agegroup == "2")
dat_younger$bmi.decile = with(dat_younger, cut(bmi,
breaks = quantile(bmi, probs = seq(0, 1, by = 0.1)),
include.lowest = T))
dat_older$bmi.decile = with(dat_older, cut(bmi,
breaks = stats::quantile(bmi, probs = seq(0, 1, by = 0.1)),
include.lowest = T))
dat_younger_melt = reshape2::melt(dat_younger, measure.vars = "bmi.decile")
dat_older_melt = reshape2::melt(dat_older, measure.vars = "bmi.decile")


ggplot(data = dat_younger,aes(x = age, y = height, color = bmi.decile)) +
geom_smooth(method = "lm", formula = y ~ x, se = F) + facet_wrap(~genderdescription) +
ggtitle("Growth curves, ages 4-5 yrs") + xlab("Age (yrs)") +
ylab("Height (cm)") + scale_color_discrete("BMI deciles")

ggplot(data = dat_older,aes(x = age, y = height, color = bmi.decile)) +
geom_smooth(method = "lm", formula = y ~ x, se = F) + facet_wrap(~genderdescription) +
ggtitle("Growth curves, ages 10-11 yrs") +
xlab("Age (yrs)") + ylab("Height (cm)") +
scale_color_discrete("BMI deciles")




