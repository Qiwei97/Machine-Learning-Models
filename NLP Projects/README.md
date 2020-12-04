## Sentiment Analysis

There are 3 different implementations for Sentiment Analysis using different libraries, namely Tensorflow (TPU compatible), Pytorch (TPU compatible) and the Huggingface Trainer API.

## Recommendation Engine

The Recommender System (Content Based Filtering) is built using the TF-IDF method. It first uses Logistic Regression to classify user text input into the correct category. After which, the Recommender system will filter and output the top 5 controls related to that category based on Cosine Similarity.

Apart from Logistic Regression, other models provided by Pycaret as well as keras neural networks have been tested out as part of the model benchmarking process.
