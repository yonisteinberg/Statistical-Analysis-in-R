Final_Project_New
Yoni Steinberg

December 9, 2016

Introduction
The NLSY79 is a nationally representative sample of 12,686 young men and women who were 14-22 years old when they were first surveyed in 1979. These individuals were interviewed annually through 1994 and are currently interviewed on a biennial basis. (bis.gov/nls/nlsy79.htm)

We’re curious to answer the question: “Is there a significant difference in incmoe between men and women? Does the difference vary depending on other factors?”

Of the 70 variables available in the data set, I choose to look at the following survey responses to support my analysis: 1) Expected Educational Attainment at time of survey: “exp.Educ1979” 2) Actual family size in 2000: “Famsize_2000” 3) Gender: “Gender” 4) Race: “Race” 5) Income in 2010: “Income” 6) Military service: “Military”

1. Data Summary
# Import Data
nlsy <- read.csv("http://www.andrew.cmu.edu/user/achoulde/94842/final_project/nlsy79/nlsy79_income.csv", header = TRUE)

# Change column names to clarify survey question name
colnames(nlsy) <- c("VERSION_R25_2012",
    "CASEID_1979",
    "BirthCountry",
    "South",
    "spoke.ForeignLang",
    "specific.ForeignLang",
    "FAM-RES_1979",
    "FAM-6_1979",
    "FAM-13C_1979",
    "R_REL-1_COL_1979",
    "exp.Educ1979",
    "Military",
    "WOMENS-ROLES_000001_1979",
    "WOMENS-ROLES_000002_1979",
    "WOMENS-ROLES_000003_1979",
    "WOMENS-ROLES_000004_1979",
    "WOMENS-ROLES_000006_1979",
    "WOMENS-ROLES_000007_1979",
    "WOMENS-ROLES_000008_1979",
    "EXP-OCC_1979",
    "EXP-9_1979",
    "race",
    "gender",
    "MARSTAT-KEY_1979",
    "FAMSIZE_1979",
    "POVSTATUS_1979",
    "POLICE-1_1980",
    "POLIC-1C_1980",
    "POLICE-2_1980",
    "ALCH-2_1983",
    "DS-8_1984",
    "DS-9_1984",
    "Q13-5_TRUNC_REVISED_1990",
    "POVSTATUS_1990",
    "HGCREV90_1990",
    "jobs.num",
    "NUMCH90_1990",
    "AGEYCH90_1990",
    "DS-12_1998",
    "DS-13_1998",
    "INDALL-EMP.01_2000",
    "CPSOCC80.01_2000",
    "OCCSP-55I_CODE_2000",
    "Q2-15B_2000",
    "num.children.2000",
    "Q13-5_TRUNC_REVISED_2000",
    "FAMSIZE_2000",
    "TNFI_TRUNC_2000",
    "POVSTATUS_2000",
    "MARSTAT-COL_2000",
    "MARSTAT-KEY_2000",
    "MO1M1B_XRND",
    "political.party",
    "ATT-POL-79_2008",
    "Q2-10B_Y_2012",
    "INDALL_EMP.01_2012",
    "OCCALL_EMP.01_2012",
    "OCCSP_55I_CODE_2012",
    "Q2_15A_2012",
    "Q12_6_2012",
    "income",
    "Q13_5_SR000001_2012",
    "Q13_5_SR000002_2012",
    "Q13_18_TRUNC_2012",
    "Q13_18_SR000001_TRUNC_2012",
    "Famsize_2012",
    "REGION_2012",
    "educ.Attain2000",
    "URBAN_RURAL_2012",
    "JOBSNUM_2012")
Next, I convert certain variables to factors and change the level names so that the data is easier to wrangle with.

# Transform and relabel gender and race variables
nlsy <- transform(nlsy, 
        exp.Educ1979 = as.factor(mapvalues(exp.Educ1979, c(1:8, 9:12,13:15, 16, 17:18), c(rep("middle_school", 8), rep("high_school", 4), rep("some_college", 3), "college", rep("graduate", 2)))),          
        gender = as.factor(mapvalues(gender, c(1,2), c("Male", "Female"))),
        race = as.factor(mapvalues(race, c(1:3), c("Hispanic", "Black", "Other"))),  
        BirthCountry = as.factor(mapvalues(BirthCountry, c(1,2), c("USA", "Foreign"))), 
        spoke.ForeignLang = as.factor(mapvalues(spoke.ForeignLang, c(1,0), c("Yes","No"))),
        specific.ForeignLang = as.factor(mapvalues(specific.ForeignLang, c(1:4), c("Spanish", "French", "German", "Other"))), 
        Military = as.factor(mapvalues(Military, c(1,0), c("Yes", "No")))
)        
## The following `from` values were not present in `x`: 2
# Subset data to include only the variables of interest
nlsy.trial <- subset(nlsy, select = c(educ.Attain2000,POVSTATUS_1990, jobs.num, POVSTATUS_2000, spoke.ForeignLang, specific.ForeignLang, exp.Educ1979, Famsize_2012, race, gender, round(income, 0), Military))
nlsy.pre <- subset(nlsy, select = c(spoke.ForeignLang, specific.ForeignLang, exp.Educ1979, Famsize_2012, race, gender, round(income, 0), Military))
nlsy <- subset(nlsy, select = c(exp.Educ1979, Famsize_2012, race, gender, round(income, 0), Military))
A look at the data to see what I’m working with

head(nlsy) 
##   exp.Educ1979 Famsize_2012  race gender income Military
## 1  high_school           NA Other Female     NA       No
## 2  high_school            3 Other Female  19000       No
## 3 some_college            2 Other Female  35000       No
## 4  high_school           NA Other Female     NA     <NA>
## 5     graduate           NA Other   Male     NA       No
## 6     graduate            5 Other   Male 105000       No
str(nlsy) 
## 'data.frame':    12686 obs. of  6 variables:
##  $ exp.Educ1979: Factor w/ 5 levels "college","graduate",..: 3 3 5 3 2 2 2 5 5 3 ...
##  $ Famsize_2012: int  NA 3 2 NA NA 5 NA 3 3 NA ...
##  $ race        : Factor w/ 3 levels "Black","Hispanic",..: 3 3 3 3 3 3 3 3 3 3 ...
##  $ gender      : Factor w/ 2 levels "Female","Male": 1 1 1 1 2 2 2 1 2 1 ...
##  $ income      : int  NA 19000 35000 NA NA 105000 NA 40000 75000 NA ...
##  $ Military    : Factor w/ 2 levels "No","Yes": 1 1 1 NA 1 1 NA 1 NA 1 ...
First we’ll look at our data graphically to see if there’s anything out of the ordinary.

# Graphically

#par(mfrow = c(2,3))
qplot(Famsize_2012, data = nlsy, main = "Family Size of Respondent", fill = Famsize_2012) + theme(plot.title = element_text(hjust = 0.5))
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## Warning: Removed 5385 rows containing non-finite values (stat_bin).


ggplot(data = nlsy, aes(x = exp.Educ1979, fill = exp.Educ1979, position = "dodge")) + geom_bar() + ggtitle("exp.Educ1979") + theme(plot.title = element_text(hjust=0.5))   


ggplot(data = nlsy, aes(x = race, fill = race, position = "dodge")) + geom_bar() + ggtitle("Race") + theme(plot.title = element_text(hjust=0.5))  


ggplot(data = nlsy, aes(x = gender, fill = gender, position = "dodge")) + geom_bar() + ggtitle("Gender") + theme(plot.title = element_text(hjust=0.5)) 


qplot(income, data = nlsy, main = "Income 2012", fill = income) + theme(plot.title = element_text(hjust = 0.5))  
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
## Warning: Removed 5662 rows containing non-finite values (stat_bin).


ggplot(data = nlsy, aes(x = Military, fill = Military, position = "dodge")) + geom_bar() + ggtitle("Military service") + theme(plot.title = element_text(hjust=0.5))


Let’s remove the topcoded values in the data for now, I will discuss further in the Methodology section

nlsy.before <- qplot(x=income, binwidth = 20000, geom = "histogram", data = nlsy)
nlsy.orig <- nlsy
nlsy <- subset(nlsy, income < nlsy$income[which.max(nlsy$income)], na.rm = TRUE)
Looking at Income variance by gender

income.gender <- ddply(nlsy, ~ gender, summarize, mean.income = mean(income, na.rm = TRUE))
income.gender
##   gender mean.income
## 1 Female    28492.32
## 2   Male    41834.09
boxP.income.gender <- qplot(gender, income, geom = "boxplot", data = nlsy, na.rm = TRUE, fill = gender)
boxP.income.gender


In our data it appears that income varies by gender. Let’s see if we can conclude that this is true for the overall population.

income.gender.Ttest <- t.test(income ~ gender, data = nlsy)
round(income.gender.Ttest$p.value, 10)
## [1] 0
A resounding YES, we get a p.value ~0 at the 0.05 level, highly significant.

If we add race into the equation, what would our data show us?

test <- ddply(nlsy, ~ race + gender, summarize, avg.income = round(mean(income, na.rm = TRUE), 0))
test
##       race gender avg.income
## 1    Black Female      24096
## 2    Black   Male      29498
## 3 Hispanic Female      26932
## 4 Hispanic   Male      38057
## 5    Other Female      31836
## 6    Other   Male      51395
plot.test <- qplot(x = race, y = income, geom = "boxplot", data = nlsy, fill = gender, na.rm = TRUE)
plot.test


It looks like the income gap between gender varies across race

Let’s look at this finding the way we did in class



Repeat with military



Repeat with exp.Educ1979



2. Methodology
inline <- count(nlsy.orig$income==max(nlsy.orig$income)) #people in our dataset that are slotted as earning
inline.1 <- inline[,2]
a) Topcoded values As we saw in our data summary section, and noted in the survey, the top 2% of earners are set to an income variable of the average of the top 2% of earners. There are 12686 people in our dataset that are slotted as earning $343830. This isn’t too helpful, as each of the high earning people are given the same value of income, which isn’t representative of their actual income. So we will ignore these topcoded values in our analysis. I also want to ignore missing data.

nlsy.before
## Warning: Removed 5662 rows containing non-finite values (stat_bin).


qplot(x=income, binwidth = 20000, geom = "histogram", data = nlsy)


b) Negative values & NA In the survey data, some of the questions were skipped by the participant or were not given in the interview. These values were coded as negative numbers. For example: Looking at the dataset, for the question that I renamed “num.children.2000, there are 3920 people who skipped the question, and 4653 that didn’t get the question in the interview. Coding these as a negative value wouldn’t make sense. Therefore I will code them as NA so that I can see clean up the data and manipulate the NA values as one set of values if needed”

c) Trials and tribulations

I explored many different variables before I settled on my final selection. Among those were a few of the language variables such as factors that would indicate respondents were immigrants or from a different culture. I spent a lot of time looking at the “spoke foreign language” and “specific foreign language” variable because I was interested if there would be effects between immigrants and also between children that were multi-lingual. Unfortunately, due to time restrictions I wasn’t able to proceed as in depth as would have lived to. Below are two interesting visuals just for fun.

ggplot(data = nlsy.pre, aes(x = spoke.ForeignLang, fill = race, position = "dodge")) + geom_bar() + ggtitle("Bilingual?") + theme(plot.title = element_text(hjust=0.5)) 


ggplot(data = nlsy.pre, aes(x = specific.ForeignLang, fill = race, position = "dodge")) + geom_bar() + ggtitle("Which Language") + theme(plot.title = element_text(hjust=0.5)) 


#code inserted up top: nlsy[nlsy < 0] <- NA
I performed various analyses that led to my results which I will explain in the findings.

3. Findings
Now that we have a feel of our data, and we’ve removed the topcoded values, let’s go back to our main question: does income vary between male and females?

While exploring our data earlier, it appeared that income varied between men and women. The difference is statistically significant. “race”, “gender”, “exp.Educ1979”, “Famsize_2012”,“Military” checking for collinearity

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = pmax(1, cex.cor * r))
}
pairs(nlsy.new, lower.panel = panel.cor)


None of my variables are highly correlated, so I am leaving them all in the model for now.

Let’s build an initial model with linear regression

nlsy.new <- na.omit(nlsy)
nlsy.lm <- lm(income ~ gender + race, data = nlsy.new)
kable(round(summary(nlsy.lm)$coef, 3), format = "markdown")
Estimate	Std. Error	t value	Pr(>|t|)
(Intercept)	19489.710	1046.384	18.626	0
genderMale	13925.887	1037.112	13.428	0
raceHispanic	7182.928	1508.835	4.761	0
raceOther	15041.290	1177.336	12.776	0
Is race a statistically significant predictor of income compared to a model of just gender?

anova(update(nlsy.lm, . ~ . -race), nlsy.lm)
## Analysis of Variance Table
## 
## Model 1: income ~ gender
## Model 2: income ~ gender + race
##   Res.Df           RSS Df    Sum of Sq      F                Pr(>F)    
## 1   4315 5172001149699                                                 
## 2   4313 4980847849608  2 191153300091 82.761 < 0.00000000000000022 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
With a p-value of 0, this test explains that there are variations in income that are not accounted for by gender. Race is a highly significant predictor of income in the model.

We will update the model by adding an interaction term between race and gender. We saw in the data summary above that there is income gap among different races and that the difference was statistically significant.

nlsy.lm.interact <- update(nlsy.lm, .~. + race*gender)
kable(round(summary(nlsy.lm.interact)$coef, 3), format = "markdown")
Estimate	Std. Error	t value	Pr(>|t|)
(Intercept)	23291.100	1271.883	18.312	0.000
genderMale	6003.418	1836.142	3.270	0.001
raceHispanic	3677.370	2077.588	1.770	0.077
raceOther	8830.921	1611.565	5.480	0.000
genderMale:raceHispanic	7298.362	3010.768	2.424	0.015
genderMale:raceOther	13224.694	2351.004	5.625	0.000
Test significance of interaction term

anova(nlsy.lm, nlsy.lm.interact)
## Analysis of Variance Table
## 
## Model 1: income ~ gender + race
## Model 2: income ~ gender + race + gender:race
##   Res.Df           RSS Df   Sum of Sq      F       Pr(>F)    
## 1   4313 4980847849608                                       
## 2   4311 4944459734022  2 36388115586 15.863 0.0000001368 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
The p-value is statistically significant at 0.000 so conclude that the income gap between men and women does vary with race.

Let’s add more of our variables to the model to see if they help in our analysis

nlsy.lm.interact.mil <- update(nlsy.lm.interact, .~. + Military)
kable(round(summary(nlsy.lm.interact.mil)$coef, 3), format = "markdown")
Estimate	Std. Error	t value	Pr(>|t|)
(Intercept)	23344.123	1272.326	18.348	0.000
genderMale	6343.186	1852.301	3.424	0.001
raceHispanic	3646.461	2077.488	1.755	0.079
raceOther	8818.028	1611.421	5.472	0.000
MilitaryYes	-3132.743	2264.412	-1.383	0.167
genderMale:raceHispanic	7280.019	3010.478	2.418	0.016
genderMale:raceOther	13146.825	2351.429	5.591	0.000
With a p.value of 0.167, we’re going to drop this variable; this is also consistent with our findings in the data summary where our error bars overlapped showing that the income gap difference, while slightly different in our dataset, was not conclusive or insightful.

Let’s keep going…

nlsy.lm.1979 <- update(nlsy.lm.interact, .~. + exp.Educ1979)
kable(round(summary(nlsy.lm.1979)$coef, 3), format = "markdown")
Estimate	Std. Error	t value	Pr(>|t|)
(Intercept)	33318.623	1458.910	22.838	0.000
genderMale	6996.280	1751.838	3.994	0.000
raceHispanic	6393.758	1994.159	3.206	0.001
raceOther	9363.438	1537.287	6.091	0.000
exp.Educ1979graduate	4619.171	1788.373	2.583	0.010
exp.Educ1979high_school	-20651.273	1247.423	-16.555	0.000
exp.Educ1979middle_school	-33929.807	3754.656	-9.037	0.000
exp.Educ1979some_college	-10186.654	1482.532	-6.871	0.000
genderMale:raceHispanic	6016.698	2873.353	2.094	0.036
genderMale:raceOther	12429.094	2242.404	5.543	0.000
anova(nlsy.lm.interact, nlsy.lm.1979)
## Analysis of Variance Table
## 
## Model 1: income ~ gender + race + gender:race
## Model 2: income ~ gender + race + exp.Educ1979 + gender:race
##   Res.Df           RSS Df    Sum of Sq      F                Pr(>F)    
## 1   4311 4944459734022                                                 
## 2   4307 4489439291710  4 455020442312 109.13 < 0.00000000000000022 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
nlsy.lm.interact.1979 <- update(nlsy.lm.1979, .~. + exp.Educ1979*gender)
kable(round(summary(nlsy.lm.interact.1979)$coef, 4), format = "markdown")
Estimate	Std. Error	t value	Pr(>|t|)
(Intercept)	33268.4194	1635.573	20.3405	0.0000
genderMale	7153.3813	2424.315	2.9507	0.0032
raceHispanic	6278.2323	2004.247	3.1325	0.0017
raceOther	9374.2593	1537.997	6.0951	0.0000
exp.Educ1979graduate	1912.0217	2487.560	0.7686	0.4422
exp.Educ1979high_school	-20009.1677	1705.643	-11.7312	0.0000
exp.Educ1979middle_school	-32402.6776	4958.363	-6.5350	0.0000
exp.Educ1979some_college	-9942.3578	1964.672	-5.0606	0.0000
genderMale:raceHispanic	6186.9224	2892.691	2.1388	0.0325
genderMale:raceOther	12343.0437	2244.682	5.4988	0.0000
genderMale:exp.Educ1979graduate	5426.8111	3581.106	1.5154	0.1297
genderMale:exp.Educ1979high_school	-1315.3396	2501.697	-0.5258	0.5991
genderMale:exp.Educ1979middle_school	-3564.4558	7591.923	-0.4695	0.6387
genderMale:exp.Educ1979some_college	-582.9251	2995.018	-0.1946	0.8457
It doesn’t appear that our interaction terms provide us with much insight with the high p-values. Let’s check with anova.

anova(nlsy.lm.1979, nlsy.lm.interact.1979)
The p. value is 0.35, so drop the interaction term.

nlsy.lm.1979.fam <- update(nlsy.lm.1979, .~. + Famsize_2012)
kable(round(summary(nlsy.lm.1979.fam)$coef, 6), format = "markdown")
Estimate	Std. Error	t value	Pr(>|t|)
(Intercept)	28691.633	1721.1833	16.669714	0.000000
genderMale	7550.436	1750.3850	4.313586	0.000016
raceHispanic	5746.341	1992.7163	2.883672	0.003950
raceOther	9313.628	1532.9997	6.075427	0.000000
exp.Educ1979graduate	4622.125	1783.3483	2.591824	0.009579
exp.Educ1979high_school	-20465.809	1244.4638	-16.445483	0.000000
exp.Educ1979middle_school	-34004.985	3744.1357	-9.082199	0.000000
exp.Educ1979some_college	-9987.483	1478.8962	-6.753336	0.000000
Famsize_2012	1797.337	357.2858	5.030529	0.000001
genderMale:raceHispanic	5728.685	2865.8513	1.998947	0.045677
genderMale:raceOther	11994.367	2237.7724	5.359958	0.000000
This is our final model

nlsy.final <- nlsy.lm.1979.fam
summary(nlsy.final)
## 
## Call:
## lm(formula = income ~ gender + race + exp.Educ1979 + Famsize_2012 + 
##     gender:race, data = nlsy.new)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -76551 -21134  -5094  17433 141590 
## 
## Coefficients:
##                           Estimate Std. Error t value             Pr(>|t|)
## (Intercept)                28691.6     1721.2  16.670 < 0.0000000000000002
## genderMale                  7550.4     1750.4   4.314      0.0000164226564
## raceHispanic                5746.3     1992.7   2.884              0.00395
## raceOther                   9313.6     1533.0   6.075      0.0000000013434
## exp.Educ1979graduate        4622.1     1783.3   2.592              0.00958
## exp.Educ1979high_school   -20465.8     1244.5 -16.445 < 0.0000000000000002
## exp.Educ1979middle_school -34005.0     3744.1  -9.082 < 0.0000000000000002
## exp.Educ1979some_college   -9987.5     1478.9  -6.753      0.0000000000164
## Famsize_2012                1797.3      357.3   5.031      0.0000005090505
## genderMale:raceHispanic     5728.7     2865.9   1.999              0.04568
## genderMale:raceOther       11994.4     2237.8   5.360      0.0000000875954
##                              
## (Intercept)               ***
## genderMale                ***
## raceHispanic              ** 
## raceOther                 ***
## exp.Educ1979graduate      ** 
## exp.Educ1979high_school   ***
## exp.Educ1979middle_school ***
## exp.Educ1979some_college  ***
## Famsize_2012              ***
## genderMale:raceHispanic   *  
## genderMale:raceOther      ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 32190 on 4306 degrees of freedom
## Multiple R-squared:  0.169,  Adjusted R-squared:  0.1671 
## F-statistic:  87.6 on 10 and 4306 DF,  p-value: < 0.00000000000000022
par(mfrow = c(1,1))
plot(nlsy.final)


4. Discussion
Our final model predicts the income gap, but as we can see from our findings section and the Normal Q-Q plot, our model is not perfect for predicting income.

R - squared Our final r-squared was 0.169 which is low. Our model is only explaining 0.169 of the variance in income. I’m less worried about this statistic however as we are not trying to figure out the factors that effect income. We are instead looking at the factors that exacerbate or mitigate the income gap. My model ended up having 10+ factors that were significant in impacting the income gap.

For fun, I choose three random variables that I just had a hunch would be more appropriate for explaining income. Just by regressing the three variable: educ.Attain2000, jobs.num, and POVSTATUS_2000, we already attain a slightly higher (.177) r-squared than our complex model. However, this isn’t helping us determine the income gap at all.

summary(lm(income ~ educ.Attain2000 + jobs.num + POVSTATUS_2000, data = nlsy.trial))
## 
## Call:
## lm(formula = income ~ educ.Attain2000 + jobs.num + POVSTATUS_2000, 
##     data = nlsy.trial)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -103491  -28232   -7793   14594  331168 
## 
## Coefficients:
##                 Estimate Std. Error t value             Pr(>|t|)    
## (Intercept)     -57953.8     4131.7 -14.027 < 0.0000000000000002 ***
## educ.Attain2000   8155.9      291.9  27.940 < 0.0000000000000002 ***
## jobs.num          -557.5      161.2  -3.459             0.000546 ***
## POVSTATUS_2000  -23629.7     2082.8 -11.345 < 0.0000000000000002 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 52120 on 5294 degrees of freedom
##   (7388 observations deleted due to missingness)
## Multiple R-squared:  0.1773, Adjusted R-squared:  0.1769 
## F-statistic: 380.4 on 3 and 5294 DF,  p-value: < 0.00000000000000022
While I developed a solid model, I’m not sure how confident I would be at presenting my results as scientific fact. In essence, this was an awesome experience for someone who only started coding this fall and has taken one statistics class in the past 3 years. I’d love to continue on with more in-depth statistical analysis training and be able to apply my new R skills to explore further.

Another interesting finding was this idea of “motivation” or “drive”. When breaking out the factor variable of expected education in 1979, it’s easy to see from our coefficients that respondents who believed that they would complete higher levels of schooling actually ended up making more income further down the road. That raises a more complicated question of resource allocation to mitigate such gaps. For instance, if you invest in early education in low income neighborhoods and raise children’s expectations, their long term income potential might go up more than just investing in something like a scholarship, or a welfare benefit.
