<!-- badges: start -->

[![DOI](https://zenodo.org/badge/687235003.svg)](https://zenodo.org/badge/latestdoi/687235003)

<!-- badges: end -->

# Brown et al. 2023 

**Matilda v1.0: An R package for probabilistic climate projections using a reduced complexity climate model**

**Target Journal**: *PLOS Climate*

Joseph K Brown*, Leeya Pressburger, Abigail Snyder, Kalyn Dorheim, Steven J Smith, Claudia Tebaldi, Ben Bond-Lamberty

*corresponding author: [joseph.brown@pnnl.gov](joseph.brown@pnnl.gov)

## Absract

A primary advantage to using reduced complexity climate models (RCMs) has been their ability to quickly conduct probabilistic climate projections, a key component of uncertainty quantification in many impact studies and multisector systems. Providing frameworks for such analyses has been a target of several RCMs used in studies of the future co-evolution of the human and Earth systems. In this paper, we present Matilda, an open-science R software package that facilitates probabilistic climate projection analysis, implemented here using the Hector simple climate model in a seamless and easily applied framework. The primary goal of Matilda is to provide the user with a turn-key method to build parameter sets from literature-based prior distributions, run Hector iteratively to produce perturbed parameter ensembles (PPEs), weight ensembles for realism against observed historical climate data, and compute probabilistic projections for different climate variables. This workflow gives the user the ability to explore viable parameter space and propagate uncertainty to model ensembles with just a few lines of code. The package provides significant freedom to select different scoring criteria and algorithms to weight ensemble members, as well as the flexibility to implement custom criteria. Additionally, the architecture of the package simplifies the process of building and analyzing PPEs without requiring significant programming expertise, to accommodate diverse use cases. We present a case study that provides illustrative results as an example of the software application for determining probabilistic projections of mean global temperature and discuss goals for future development.

