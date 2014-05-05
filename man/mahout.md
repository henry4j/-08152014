* https://mahout.apache.org/
* https://mahout.apache.org/users/basics/algorithms.html
  * Collaborative Filtering
  * User-Based Collaborative Filtering - single machine
  * Item-Based Collaborative Filtering - single machine / MapReduce
  * Matrix Factorization with Alternating Least Squares - single machine / MapReduce
  * Matrix Factorization with Alternating Least Squares on Implicit Feedback- single machine / MapReduce
  * Weighted Matrix Factorization, SVD++, Parallel SGD - single machine
  * RowSimilarityJob


#### User-based Recs

* Mahout recommenders expect interations between user and items as input; a textfile, where each lines has an interaction.
* the idea behind user-based recs. is to find users with similar tastes, and pick recs. from their items.
  * one popular method is to compute correlation coeff. between user interactions.
* 
* the following:
* `userId,itemId,value` # the value denotes the strength of the interaction, e.g. the rating given to the movie.

```java
DataModel model = new FileDataModel(new File("/path/to/dataset.csv"));
UserSimilarity similarity = new PearsonCorrelationSimilarity(model);
UserNeighborhood neighborhood = new ThresholdUserNeighborhood(0.1, similarity, model);
```
