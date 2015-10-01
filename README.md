# README

2015-10-01 kiy (Kiyoshi YONEDA) kiyoneda(at)me.com

## Purpose

This is a collection of programs for decision making.

The aim has been to automate decision making using resource-constrained devices. 
The programs are in R at the moment, but simple enough to port to smaller languages without optimization libraries.
For a quick look at what can be done, please take a look at the [beer and peanuts](doc/en/beerpeanuts.html), a minimal example.
The model also enables probabilistic forecast of individual behavior.

## Input

1. Causality relationship $\mathrm{f}: x \mapsto y$ where $x$ and $y$ are vectors of decision variables and outcomes, respectively.
2. For all outcomes $y_i$, itemwise action rate $\mathrm{p}_i(y_i)$ in the form of a function table.

## Method

$\hat{y} := \arg \max_x \mathrm{P}(y) = \arg \max_x \prod_i \mathrm{p}_i[\mathrm{f}(x)]$.

A bijection $y \mapsto z$ normalizes $y$ into dimensionless $z$; optimization is carried out in $z$ rather than $y$.

## Output

1. Optimal outcomes $\hat{y}_i$.
2. Action rate $\mathrm{P}(\hat{y}) := \prod_i \mathrm{p}_i(\hat{y}_i)$ which may be interpreted as the probability that the decision maker takes the action resulting in $\hat{y}_i$.
3. A table of $\{[\hat{z}_i, \mathrm{p}_i({\hat{z}_i})]\}$, which may be put into a single graph.