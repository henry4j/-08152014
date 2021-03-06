Keywords: observations, [SGD (logistic regression)](http://cwiki.apache.org/confluence/display/MAHOUT/Logistic+Regression), loss function, inference, ...

### TOC

* [ML introduction](#ml-introduction)
* [Modeling WF](#modeling-wf)
  * [Problem definition](#ml-problem-definition)
  * [Data Collection & Integration](#data-collection)
  * [Data Preparation & Clean up](#data-preparation)
  * [Data Visualization & Analysis](#data-analysis)
  * [Feature Engineering](#feature-engineering)

### ML introduction

ML is programming by relationships or patterns in data.
ML algorithms constructs mathematical models of data to discover patterns.
ML models are subsequently used to make decisions or predictions on unseen data.
e.g. we use ML models out of purchase, browsing, and search histories to predict if a buyer will purchase a product.

Why ML? handwriting & speech recognitions cannot be adequately solved by rule-based algorithms.
ML by patterns in data is much more effective/scalable when data/patterns are personalized, large in volume, or changing in time dynamically.

Note Mahout has algorithms on 3C: collaborative filtering, classification, and clustering that are also called recommender, supervised learning, and unsupervised learning.

#### Supervised ML

We can formulate many of ML problems in terms of prediction of a desired target property of an object, e.g. if a website is e-commerce or not, given other known properties of the object (e.g. text, hyperlinks).
Supervised ML approach consists of two key phases - (1) training, and (2) prediction -- training or model building phase involves learning a model or patterns from historical data (e.g. positive & negative examples of e-commerce sites), and prediction phase uses the learned model to determine the target property.  

##### Classification vs. Regression

In classification problems, the target property takes discrete values (e.g., whether a website is e-commerce or not),  
while in regression problems, the target property is continuous (e.g., the demand forecast for a product). 

### Modeling WF

1. ML problem defintion
2. Data collection & integration
3. Data preparation & cleaning
4. Data visualization & analysis
5. Feature engineering
6. Model training & parameter tuning
7. Model evaluation
8. Model deployment

#### ML problem definition

##### Key Elements of Machine Learning Problem

Machine learning (ML) problems have three main elements: observation, feature, and label.

##### Observation

An observation (example, or instance) is the object that is being learned from and predicted on.
When detecting fraudulent orders placed on retail sites, orders are the objects which need to be 
determined as fraud or non-fraud, and each order constitutes an observation for the problem.
When classifying products into appropriate categories, each individual product is the object 
being trained on and scored, and constitutes a single observation for that problem.

##### Feature

Usually, an observation is described by a set of features, also known as attributes or variables.
e.g., the features of an order include the date/time, the product, the cost, the buyer, the shipping cost, and so on.
The features of a product are its title, description, price, brand, model #, average # of glance views per day, # of reviews, etc.
Features can have different data types: numeric (real valued), binary (two values), categorical (multiple values) or unstructured text.

##### Label

Label (or target property variable) is the value that we are trying to predict. Labels are a special requirement for Supervised ML, 
as we learn from the values in the historical data.

In fraud detection problem, the labels would be binary indicators in the historical data,
which indicate whether an order was determined to be fraudulent or not.
In product classification problem, the training data would contain products which have been categorized using rules or manual review.
The assigned category for each product would then constitute the label for this problem.

Note that labels must be assigned to observations in the historical data which is used for training and testing a model.
The training algorithm tries to learn the best model that can accurately predict the (known) label for maximum number of observations during the testing phase. When using the model for predictions, we only need observations with corresponding features without the labels. The model will then predict the label based on the learned parameters.

##### Income Classification Problem

Let's categorize high-income individuals in census data - predict whether the income for an individual exceeds $50K annually based on census data.
This is a standard dataset made available by UCI machine learning repository (originally from 1994 US Census), and used extensively by ML community to experiment different algorithms. See more details at http://goo.gl/sEUK3J.

Below are the features with their data types:

Feature | Description | Data Type
--- | --- | ---
income class id (Label) | Whether income is greater or less than $50,000. | Binary {`<50K` or `>=50K`}
age	| Age of the individual. |	Numeric
workclass | Working class of the individual. |	Categorical {Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked}
final-weight | Weighted estimate across different socio-economic characteristics. |	Numeric
education | Education level of the individual. |	Categorical {Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool}
education-num | Number of years of education of the individual. |	Numeric
marital-status | Marital status of the individual. |	Categorical {Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse}
occupation | Occupation of the individual. | Categorical {Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces}
relationship | Relationship. |	Categorical {Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried}
race | Race of the individual. |	Categorical {White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black}
gender	| Gender of the individual. |	Categorical {Female, Male}
capital-gain | Capital gain.	| Numeric
capital-loss | Capital loss. |	Numeric
hours-per-week | Number of hours worked per week. |	Numeric
native-country | Individual's native country. | Categorical { US, ... }

Below are two observations from the dataset:

Label	| Age	| Work class	| Education	| Years of education	| Marital status	| Occupation	| Relationship	| Race	| Sex	| Capital gain	| Capital loss	| Hours per week	| Native country
--- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | ---
<50K	| 39	| State-gov	| Bachelors	| 13	| Never-married	| Adm-clerical	| Not-in-family	| White	| Male	| 2174	| 0	| 40	| United-States
>=50K	| 31	| Private	| Masters | 13	| Married	| Prof-specialty	| Not-in-family	| White	| Female	| 14084	| 0	| 50	| United-States


##### Sidebar: Is it an ML problem? 

It is important to remember that ML is not a silver bullet. There are certain use cases where simple solutions can be developed without using ML techniques.
For example, ML is not needed when the target property value to be determined can be obtained using simple mappings, rules, or computation which are relatively static.
For example, obtaining the address or phone number of a customer, calculating order volume or GMS value for each customer, or determining if a customer has a new account.
Similarly, ML is not required to model processes that are highly deterministic, such as the workflow of a customer of placing an order on retail sites.
Such a process has fixed, predetermined steps which can be programmed without any data-driven learning needed.

#### Data collection

* income classification datasets for training & test from UCI machine learning repository

```bash
[ ! -e "$MAHOUT_WORK/adult.data" ] curl -o "$MAHOUT_WORK/adult.data" -kL http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data
[ ! -e "$MAHOUT_WORK/adult.data" ] curl -o "$MAHOUT_WORK/adult.text" -kL http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test
```

#### Data Preparation

##### Clean Ups

```
awk 'BEGIN { FS=", " } {printf $NF; for (i=1; i<NF; i++) if (i != 3) printf ",%s", $i; printf "\n"}' "$MAHOUT_WORK/adult.data" |
sed -e "s/^\s*<=50K/0/" -e "s/^\s*>50K/1/" |
sed '1 i\
class,age,workclass,education,education-num,marital-status,occupation,relationship,race,sex,capital-gain,capital-loss,hours-per-week,native-country\
' > "$MAHOUT_WORK/income-train-data-0.csv"

awk 'BEGIN { FS=", " } {printf $NF; for (i=1; i<NF; i++) if (i != 3) printf ",%s", $i; printf "\n"}' "$MAHOUT_WORK/adult.test" |
sed -e "s/^\s*<=50K/0/" -e "s/^\s*>50K/1/" |
sed '1 i\
class,age,workclass,education,education-num,marital-status,occupation,relationship,race,sex,capital-gain,capital-loss,hours-per-week,native-country\
' > "$MAHOUT_WORK/income-test-data-0.csv"
```

##### Imputation (substituting missing data) -- <sub>http://en.wikipedia.org/wiki/Imputation_(statistics)</sub>

Attribute values for some instances might be missing from the training and test data files. There are several strategies for handling missing values - these include:

* Replacing each missing value with a fixed constant.
* Substituting missing values with the mean of observed values for numeric attributes and the mode (most frequently occurring value) for categorical attributes.
* Using the attribute values of the nearest neighbors.
* Employing regression-based imputation.

Note the missing value script computes the mean (for numeric features), and mode (for categorical features), based only on examples in the training file. These values are then used to fill in missing values in both the training and the test data. The above strategy for dealing with missing values may not be appropriate for all applications and you should choose an appropriate strategy based on the specific application needs.

##### Randomly Shuffle Training Dataset

You can randomly shuffle the order of examples in the training set to improve the prediction accuracy of models. This is true, especially for algorithms that learn from one observation at a time (e.g. SGD in online learning), rather than algorithms that learn from the entire batch of training examples.

To randomly order the training examples, the following command adds a random number to each example in the first column (excluding the header, which is assigned -1), sorts the examples based on the column value and then strips off the column.

```bash
awk '{printf "%s,%s\n", (NR==1 ? -1 : rand()), $0}' "$MAHOUT_WORK/income-train-data-0.csv" |
sort --key 1 --numeric-sort | sed 's/^[^,]*,//' | tee "$MAHOUT_WORK/income-train-data.csv"
```

##### Additional Notes

Here are four additional data preparation and cleansing operations that you can apply to datasets:

* Outliers: Outliers are values that are dissimilar from the rest of the data and can adversely impact model performance. You can detect outliers by looking at histograms and box plots (see the next lab). The decision to remove outliers is subjective - before you remove them, you should consider if they are genuine or erroneous. Some algorithms like those based on decision trees are, in general, more resilient to outliers. You may decide to remove the particular instance, or change the particular value using any of the strategies for missing data (mentioned above).
* Feature Scaling: Feature scaling is widely used to normalize attribute values in machine learning algorithms. For a numeric attribute, feature scaling is implemented by subtracting the attribute mean from each attribute value and then dividing the result by the standard deviation: feature scaling rescales attribute values so that they have zero-mean and unit-variance, and fall in similar ranges – this ensures faster convergence of gradient descent-based learning algorithms.
* Downsampling: To handle extremely large datasets, certain ML algorithms train models on random samples that are much smaller in size. Here, you can preserve class proportions in the sample using techniques such as stratified sampling. In many applications (e.g. online advertising), the class distribution is imbalanced with skewed occurrence of one class (e.g. no-clicks). In such cases, the model training algorithm may give more importance to the majority class and ignore the minority class when learning model parameters. One strategy to deal with imbalanced classes is to downsample the dominant class. Here, we create a new training dataset that retains all examples belonging to the minority class and a sample containing an equal number of examples from the majority class. We then train a predictive model against the new training dataset with a balanced class distribution.
* Importance Weights: Instead of downsampling the majority class, a different strategy is to assign an importance weight to each example from the minority class – to ensure a balanced class distribution, the importance weight is selected as the ratio of the majority and minority classes. Thus, importance weights are a useful mechanism for correcting class imbalance without adding or removing examples from the training data. In addition to balancing class distributions, importance weights are also a useful tool for meeting specific application requirements. For instance, in some applications like adult content detection, there is a significant cost associated with misclassifying a positive example and so one can penalize misclassification of positive examples by assigning them higher importance weights.

#### Data Analysis

* prerequisites: install R http://cran.rstudio.com/bin/macosx/, and R Studio http://www.rstudio.com/ide/

##### feature & target summaries

* helps detect outliers and skew in feature or target data distribution.

```bash
R
install.packages('ggplot2')
install.packages('ROCR')

? read.csv
income <- read.csv('/workspace/mahout-work/income-train-data.csv', header=T, strip.white=T) # 
income$class <- as.factor(income$class)

summary(income) # shows the statistic summary on the entire dataset.
str(income) # shows the structure of the data object.
```

```bash
summary(income)
class    age            workclass               education
0:24720  Min.   :17.00  Private:         24532  HS-grad     :10501
1: 7841  1st Qu.:28.00  Self-emp-not-inc: 2541  Some-college: 7291
         Median :37.00  Local-gov:        2093  Bachelors   : 5355
         Mean   :38.58  State-gov:        1298  Masters     : 1723
         3rd Qu.:48.00  Self-emp-inc:     1116  Assoc-voc   : 1382
         Max.   :90.00  Federal-gov:       960  11th        : 1175
                        (Other):            21  (Other)     : 5134

education.num  marital.status                occupation
Min.   : 1.00  Divorced:               4443  Prof-specialty:  5983
1st Qu.: 9.00  Married-AF-spouse:        23  Craft-repair:    4099
Median :10.00  Married-civ-spouse:    14976  Exec-managerial: 4066
Mean   :10.08  Married-spouse-absent:   418  Adm-clerical:    3770
3rd Qu.:12.00  Never-married:         10683  Sales:           3650
Max.   :16.00  Separated:              1025  Other-service:   3295
               Widowed:                 993  (Other):         7698
...
```

```bash
str(income)
'data.frame': 32561 obs. of 14 variables:
$ class : Factor w/ 2 levels "0","1": 1 1 1 1 2 1 1 2 1 2 ...
$ age : int 46 60 19 39 34 32 42 66 34 63 ...
$ workclass : Factor w/ 8 levels "Federal-gov",..: 1 4 4 4 4 4 4 5 4 4 ...
$ education : Factor w/ 16 levels "10th","11th",..: 12 8 12 10 16 12 9 12 12 10 ...
$ education.num : int 9 12 9 13 10 9 11 9 9 13 ...
$ marital.status: Factor w/ 7 levels "Divorced","Married-AF-spouse",..: 1 1 5 3 3 5 1 3 3 3 ...
$ occupation : Factor w/ 14 levels "Adm-clerical",..: 1 8 6 13 4 1 10 12 10 12 ...
$ relationship : Factor w/ 6 levels "Husband","Not-in-family",..: 5 2 3 1 6 5 5 1 1 1 ...
$ race : Factor w/ 5 levels "Amer-Indian-Eskimo",..: 3 5 5 5 5 3 5 5 5 5 ...
$ sex : Factor w/ 2 levels "Female","Male": 1 1 2 2 1 1 1 2 2 2 ...
$ capital.gain : int 0 0 0 0 0 2977 0 0 0 0 ...
$ capital.loss : int 0 0 0 0 1887 0 0 1825 0 1848 ...
$ hours.per.week: int 40 40 30 50 40 35 40 10 40 40 ...
$ native.country: Factor w/ 41 levels "Cambodia","Canada",..: 39 39 39 39 39 39 39 39 39 39 ...
```

##### feature-target correlations

* helps keep features with high correlation since they are ones with signal, and filter out features with low correlation since they are most likely noisy.

```
library('ggplot2')
qplot(data=income, x=age, main="Histogram of Age", binwidth=3)
qplot(data=income, x=age, main="Class-wise Histogram of Age", binwidth=3, position="dodge", fill=class)
qplot(data=income, x=age, main="Histogram of Age")
```

* Things stand out in the histogram:
  * on average, older people make more money -- do you see how we arrived at this conclusion?
  * relatively fewer people make more than 50K/year -- we saw this in the summary command.

<img src='http://dl.dropboxusercontent.com/u/47820156/img/histogram-class-fill.png' />

* Obtaining raw class distribution for an attribute
  * age.sal.xtab[,1]
  * age.sal.xtab[,1]/sum(age.sal.xtab[,1])
  * plot(age.sal.xtab[,1])
  * points(age.sal.xtab[,2],col='red',pch=19

```bash
age.sal.xtab=xtabs(~age+class, data=income)
age.sal.xtab

      class
age   0   1
 17 395   0
 18 550   0
 19 710   2
 20 753   0
 21 717   3
 22 752  13
 23 865  12
 24 767  31
 25 788  53
 26 722  63
 27 754  81
 28 748 119
 29 679 134
 30 690 171
 31 705 183
```

```bash
edu.sal.xtab=xtabs(~education.num+class, data=income)
matplot(100*apply(edu.sal.xtab,2,function(x){x/margin.table(edu.sal.xtab,1)}) ,type='l',lwd=2,xlab="Education",ylab="% of data")
```

##### feature-feature correlations

* helps reduce data dimensionality by selecting one of redundant features.

```
qplot(education.num, hours.per.week, data=income, color=class) + stat_smooth()
```

<img src="http://dl.dropboxusercontent.com/u/47820156/img/scatter-plot-smoother-overlaid.png" />

* there is a wide range of hours worked per-week values for every value of educationnum. we can conclude that there is little correlation between education level and the number of hours people work per week.
* furthermore, the smoother curves inch upwards towards the right of the plots, indicating that higher education levels positively correlate with more hours worked per week. the effect is more pronounced for the lower income group than the higher income group (since the red curve inches upwards more sharply). one more observation from the smoother curves is that higher income groups generally work longer hours regardless of their education level. note that these are still correlations; it is not possible to uncover a causal relationship between Income Indicator and education or hours worked from these plots.

#### Feature Engineering

* manipulating raw, original data into new, more useful representations, or features requires lots of trial and error combined with domain knowledge and ingenuity.
* often times, linear models w/ simple features may not be adequate for capturing complex correlations between the data and the target label.
* a way to improve the expressive power of linear models is to introduce non-linearity through feature transformations.
  * non-linear data transformations: numeric feature binning, or cartesian product of features.
  * domain-specific features: text features, features that capture web page structures, special features for image data.
  * data-driven features: meta-features from clusters within the data.
  * feature selection: selecting a subset of relevant features; retain features that are highly correlated w/ the target, and eliminate noisy features, or make learning computationally feasible.

```bash
types.binary:class
types.categorical:workclass,education,education-num,marital-status,occupation,relationship,race,sex,native-country
types.numeric:hours-per-week,age,capital-gain,capital-loss

hours-per-week:quantile_bin:10
age:quantile_bin:10
capital-loss:quantile_bin:10
capital-gain:quantile_bin:10

processor.cartesian.1: relationship, marital-status
processor.cartesian.2: relationship, (age:quantile_bin:10)
processor.cartesian.3: relationship, occupation
processor.cartesian.4: marital-status, (age:quantile_bin:10)
processor.cartesian.5: marital-status, occupation
processor.cartesian.6: (age:quantile_bin:10), education
processor.cartesian.7: (age:quantile_bin:10), (capital-gain:quantile_bin:10)
processor.cartesian.8: (age:quantile_bin:10), (hours-per-week:quantile_bin:10)
processor.cartesian.9: (age:quantile_bin:10), sex
processor.cartesian.10: occupation, education
processor.cartesian.11: education, (hours-per-week:quantile_bin:10)
```

#### Model Training

```bash
cat <<EOF >> "/tmp/income_params.txt"
PredictionType:binary
Passes:10
EOF
```

```bash
3ml create predictor \
--labelColumn class \
--recipe ./income_recipe.txt \
--dataFileType CSV \
--trainingParams ./income_params.txt \
--trainingFile s3://3ml-training-data/work_id_1234/income_train_data.csv
```

