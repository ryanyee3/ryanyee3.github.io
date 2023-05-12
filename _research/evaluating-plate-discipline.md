---
title: "Evaluating plate discipline in Major League Baseball using Bayesian Additive Regression Trees"
---

*Abstract*
We introduce a three-step framework to determine, on a per-pitch basis, whether batters in Major League Baseball should swing at a pitch. Unlike traditional plate discipline metrics, which implicitly assume that all batters should always swing (resp. take) pitches inside (resp. outside) the strike zone, our approach explicitly accounts not only for the players and umpires involved but also in-game contextual information like the number of outs, the count, baserunners, and score. Specifically, we first fit flexible Bayesian nonparametric models to estimate (i) the probability that the pitch is called a strike if the batter takes the pitch; (ii) the probability that the batter makes contact if he swings; and (iii) the number of runs the batting team is expected to score following each pitch outcome (e.g. swing and miss, take a called strike, etc.). We then combine these intermediate estimates to determine whether swinging increases the batting team's run expectancy. Our approach enables natural uncertainty propagation so that we can not only determine the optimal swing/take decision but also quantify our confidence in that decision. We illustrate our framework using a case study of pitches faced by Mike Trout in 2019.

