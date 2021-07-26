# Elo-based Predictive Power (EPP)

This repository contains data and code necessary to reproduce figures from the article: Gosiewska A., Woźnica K., Biecek P. (2021). Interpretable Meta-Measure for Model Performance. arXiv:1908.09213, [https://arxiv.org/abs/2006.02293](https://arxiv.org/abs/2006.02293).


## Abstract

> Benchmarks for evaluation of model performance play an important role in Machine Learning. However, there is no established way to describe and create new benchmarks, what is more, the most common benchmarks use performance measures that share several limitations. For example, the difference in performance for two models has no probabilistic interpretation, there is no reference point to indicate whether they represent a significant improvement, and it makes no sense to compare such differences between data sets. We introduce a~new meta-measure assessment named Elo-based Predictive Power (EPP) that is built on top of other performance measures. The differences in EPP scores have a probabilistic interpretation and can be directly compared between data sets, Furthermore, the logistic regression-based design allows for an assessment of ranking fitness based on deviance statistic. We prove the mathematical properties of EPP and support them with empirical results of a large scale benchmark on 30 classification data sets and real-world benchmark for visual data. Additionally, we propose a Unified Benchmark Ontology that is used to a uniform description of benchmarks.

## Diagram of the EPP Benchmark

![](./figures/figure_2_EPP_diagram.png)

|    Component   |                                                       Description                                                      |                Example                |
|--------------|----------------------------------------------------------------------------------------------------------------------|-------------------------------------|
| Player_{i}     | A single i-th participant of the EPP Benchmark.                                                                        | Classification model                  |
| Score          | A one-dimensional measure of Player’s  strength. We assume that the order  relation over Scores is given and monotonic | Accuracy                              |
| Round_{r}      | A single game environment for Players. The  outcome of a Round_{r} are score values of  Players.                       | Cross-validation field                |
| Tournament     | An independently replicated Rounds.                                                                                    | Data set                              |
| Meta-Score     | A measure of a Player’s strength aggregated  over all Rounds in a Tournament.                                          | Mean                                  |
| Leaderboard    | The ordering of Players according to their  overall strength on all Rounds in a Tournament.                            | Mean accuracy of models over CV folds |
| Opponent_{i,j} | Player_{j}, whose Score values are compared to the Score values of the Player_{i}.                                     | Classification model                  |
| Match_{i,j,r}  | A single comparison of the Score values of a pair  of Players, i.e. Player_{i} and Opponent_{i,j} in  Round_{r}.       |                                       |                                 |
