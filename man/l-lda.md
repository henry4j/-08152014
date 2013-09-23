### corpus: 2285 labeled documents

* split-comm-text -f 0.75,0.25 $MAHOUT_WORK/test-set-2285-labeled_typos.csv

tc# | train dataset | test dataset | accuracy
--- | --- | --- | ---
tc1 | rrc_pro_2285_labeled_1.0.csv (2285 x 284) | rrc_pro_2285_labeled_1.0.csv | 8.8% (1800 / 2285)
tc2 | rrc_pro_2285_labeled_1.0.csv (2285 x 284) | rrc_pro_2285_labeled_0.75.csv | 78.5% (1349 / 1719)
tc3 | rrc_pro_2285_labeled_1.0.csv (2285 x 284) | rrc_pro_2285_labeled_0.25.csv | 79.8% (451 / 565)
tc4 | rrc_pro_2285_labeled_0.75.csv (1718 x 236) | rrc_pro_2285_labeled_1.0.csv | 45.1% (1031 / 2285)
tc5 | rrc_pro_2285_labeled_0.75.csv (1718 x 236) | rrc_pro_2285_labeled_0.75.csv | 45.3% (779 / 1719)
tc6 | rrc_pro_2285_labeled_0.75.csv (1718 x 236) | rrc_pro_2285_labeled_0.25.csv | 46.2% (261 / 565)



### l-lda ideas

Ref: http://aclweb.org/anthology//D/D09/D09-1026.pdf

l-lda
* outperforms SVMs by more than 3 to 1 when extracting tag-specific document snippets.
* competitive as a multi-level classifier on various data sets.

Let each document d be represented by a tuple of
* a list of word indices: w<sup>(d)</sup> = (w1, ..., w<sub>_Nd_</sub>)
* a list of topic presenses: Λ<sup>(d)</sup> = (l1, ..., l<sub>_k_</sub>)  
  where N<sub>d</sub>: the document length, and K: the # of unique labels.
