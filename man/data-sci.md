#### Naive Bayes

* Bayes Law
  * 1% of people is sick; 99% of sick ppl. test +, and 99% of healthy ppl. test -.
  * given a patient tests positive, what is the p the patient is actually sick?
  * P(sick|+) = P(sick ∩ +) / P(+) = (1% * 99%)/(1% * 99%+ + 99% * 1%+) = 50%.
* P(spam|word) = P(word ∩ spam) / P(word)?
  * p_spam, p_ham = 1500 / (1500 + 3672), 3672 / (1500 + 1672) # given 1500 spam and 3672 ham.
  * P(meeting|spam) = 16 / 1500 # 16 spams and 153 hams with "meeting".
  * P(spam|meeting) = (16 / 1500) / ((16 + 153) / (1500 + 3672)) = 9%
  

