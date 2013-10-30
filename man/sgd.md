##### [Logistic Regression (SGD)](https://cwiki.apache.org/confluence/display/MAHOUT/Logistic+Regression)

* http://ashokabhat.wordpress.com/2012/02/13/mahout-and-hadoop-run-the-logistic-regression/
* http://bickson.blogspot.com/2011/01/mahout-on-amazon-ec2-part-2-testing.html
* http://imiloainf.wordpress.com/2011/11/02/mahout-logistic-regression/
* http://jayatiatblogs.blogspot.com/2013/05/running-mahouts-logistic-regression.html
* http://sujitpal.blogspot.com/2012/09/learning-mahout-classification.html
* http://www.cnblogs.com/batys/p/3295942.html
* http://www.datastax.com/dev/blog/apache-mahout-in-datastax-enterprise-building-a-classification-system

#
* http://svn.apache.org/repos/asf/mahout/trunk/examples/src/main/resources/donut.csv
* /workspace/mahout/examples/bin/classify-20newsgroups.sh
* mahout trainlogistic
* mahout runlogistic

#

```bash
curl -o $MAHOUT_WORK/donut.csv \
  -kL http://svn.apache.org/repos/asf/mahout/trunk/examples/src/main/resources/donut.csv
curl -o $MAHOUT_WORK/donut-test.csv \
  -kL http://svn.apache.org/repos/asf/mahout/trunk/examples/src/main/resources/donut-test.csv

mahout trainlogistic \
  --passes 100 --rate 50 --lambda 0.001 --input $MAHOUT_WORK/donut.csv --features 21 \
  --output $MAHOUT_WORK/donut.model --target color --categories 2 --predictors x y xx xy yy a b c --types n n
21
color ~ 
0.353*Intercept Term + 5.450*a + 2.765*b + -24.161*c + 5.450*x + -4.740*xx + 0.353*xy + -1.671*y + 0.353*yy
      Intercept Term 0.35319
                   a 5.45000
                   b 2.76534
                   c -24.16091
                   x 5.45000
                  xx -4.73958
                  xy 0.35319
                   y -1.67092
                  yy 0.35319
    2.765337737     0.000000000    -1.670917299     0.000000000     0.000000000     0.000000000     5.449999190     0.000000000   -24.160908591    -4.739579336     0.353190637     0.000000000     0.000000000     0.000000000     0.000000000     0.000000000     0.000000000     0.000000000     0.000000000     0.000000000     0.000000000

mahout runlogistic \
  --input $MAHOUT_WORK/donut-test.csv --model $MAHOUT_WORK/donut.model \
  --auc --scores --confusion
"target","model-output","log-likelihood"
0,0.004,-0.003696
0,0.003,-0.002722
1,0.959,-0.042384
1,0.977,-0.023617
0,0.000,-0.000166
1,0.922,-0.081457
1,0.678,-0.388569
0,0.160,-0.174764
0,0.019,-0.019335
0,0.740,-1.348002
0,0.040,-0.040603
1,0.873,-0.135365
1,0.106,-2.242013
1,0.933,-0.069273
1,0.997,-0.003449
0,0.106,-0.112158
1,0.971,-0.029869
0,0.001,-0.001182
1,0.898,-0.107512
0,0.000,-0.000007
0,0.103,-0.108486
0,0.033,-0.034022
0,0.003,-0.003357
0,0.722,-1.281526
0,0.002,-0.002285
1,0.997,-0.002749
1,0.968,-0.032817
0,0.013,-0.013217
0,0.458,-0.613088
0,0.020,-0.019809
0,0.563,-0.827950
0,0.178,-0.195591
0,0.340,-0.416144
0,0.043,-0.043604
0,0.020,-0.020153
0,0.088,-0.091683
1,0.649,-0.432606
0,0.832,-1.786718
0,0.007,-0.006844
0,0.014,-0.014132
AUC = 0.96
confusion: [[23.0, 1.0], [4.0, 12.0]]
entropy: [[-0.2, -2.3], [-4.2, -0.2]]
```

```bash
Usage:
  --quiet
  --input input
  --output output
  --features numFeatures  the number of hashed features to use

  --noBias                don't include a bias term
  --passes passes         the number of times to pass over the input data
  --rate learningRate     the learning rate
  --lambda lambda         the amount of coefficient decay to use

  --types t [t ...]       a list of predictor variable types (numeric, word, or text)
  --predictors p [p ...]  a list of predictor variables
  --categories number     the number of target categories to be considered
  --target target         the name of the target variable
```
