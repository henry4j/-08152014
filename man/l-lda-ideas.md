# l-lda

Ref: http://aclweb.org/anthology//D/D09/D09-1026.pdf

l-lda
* outperforms SVMs by more than 3 to 1 when extracting tag-specific document snippets.
* competitive as a multi-level classifier on various data sets.

Let each document d be represented by a tuple of
* a list of word indices: w<sup>(d)</sup> = (w1, ..., w<sub>_Nd_</sub>)
* a list of topic presenses: Λ<sup>(d)</sup> = (l1, ..., l<sub>_k_</sub>)  
  where N<sub>d</sub>: the document length, and K: the # of unique labels.
