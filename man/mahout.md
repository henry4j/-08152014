* https://mahout.apache.org/
* https://mahout.apache.org/users/basics/algorithms.html
  * Collaborative Filtering
  * User-Based Collaborative Filtering - single machine
  * Item-Based Collaborative Filtering - single machine / MapReduce
  * Matrix Factorization with Alternating Least Squares - single machine / MapReduce
  * Matrix Factorization with Alternating Least Squares on Implicit Feedback- single machine / MapReduce
  * Weighted Matrix Factorization, SVD++, Parallel SGD - single machine
  * RowSimilarityJob


#### Goodby MapReduce and Hello [Spark](http://spark.apache.org/)

#### [Recommender](https://builds.apache.org/job/mahout-quality/javadoc/org/apache/mahout/cf/taste/recommender/Recommender.html) -- <sub>estimatePreference, recommend, setPreference, and removePreference, getDataModel.</sub>

#### User-based Recs

* Mahout recommenders expect interations between user and items as input; a textfile, where each lines has an interaction.
* the idea behind user-based recs. is to find users with similar tastes, and pick recs. from their items.
  * one popular method is to compute correlation coeff. between user interactions.
* evaluation/metrics: A/B test, or a statistical offline, or a hold-out test (90% for training and 10% for testing). 
* `userId,itemId,value` # the value denotes the strength of the interaction, e.g. the rating given to the movie.

```java
DataModel model = new FileDataModel(new File("/path/to/dataset.csv")); // loads user interactions.
UserSimilarity sims = new PearsonCorrelationSimilarity(model); // computes correlation coeff.
UserNeighborhood neighborhood = new ThresholdUserNeighborhood(0.1, sims, model); // defines which similar users to leverage.
UserBasedRecommender recommender = new GenericUserBasedRecommender(model, neighborhood, sims);
List recommendations = recommender.recommend(2, 3); // 3 recomendations for user 2.
```

#### Item-based Recs

* SIMILARITY_LOGLIKELIHOOD

```bash
mahout recommenditembased -s SIMILARITY_LOGLIKELIHOOD -i /path/to/input/file -o /path/to/desired/output --numRecommendations 25
```
