#### Naive Bayes

* Bayes Law
  * 1% of people is sick; 99% of sick ppl. test +, and 99% of healthy ppl. test -.
  * given a patient tests positive, what is the p the patient is actually sick?
  * P(sick|+) = P(sick ∩ +) / P(+) = (1% * 99%)/(1% * 99%+ + 99% * 1%+) = 50%.
* P(spam|word) = P(word ∩ spam) / P(word)?
  * P(spam|meeting) = (16 / 5172.0) / (0.29 * 0.0106 + 0.71 * 0.0416) = 9%
    P(spam), P(ham) = 0.29, 0.71 # from 1500 spams and 3672 hams.
    P(meeting|spam), P(meeting|ham) = 16/1500, 153/3672 # 16 spams and 153 hams w/ meeting.
  * P(spam|money) = 80%; P(spam|viagra) = 100%, P(spam|amazon) = 0%.
