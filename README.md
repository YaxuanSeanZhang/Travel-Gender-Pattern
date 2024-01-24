# Travel Gender Pattern

## Overview
This repository contains code and data analysis for research examining how gender intersects with other identities to create distinct travel behavior patterns and needs in daily life. 
The study uses survey data from Minnesota through the [Daynamica app](https://daynamica.com/smartphone-app/) and applies innovative analytical techniques including sequence alignment and machine learning algorithms.

Additionally, this repository includes visualization and analysis components tailored for paper publication. 
The associated paper has been under review at the journal Travel Behavior and Society.

## Files 
The repository contains the following key files and scripts:

* `Step1-Cleaning.R`: This script includes data reading, cleaning, and formatting (e.g., reclassifying the activity types and travel modes).
* `Step2-CreateSeq.R`: This script creates sequences of trips and activities, defines a gender-based cost matrix, and measures the similarity of daily schedules.
* `Step3-Clustering.R`: This script includes data clustering and visualization to group people with similar travel behaviors based on the distances between sequences.
* `Step4_Vis-Stat.R`: This script compares gendered patterns across key subgroups and applies the CHAID analysis to detect which personal attributes explain travel patterns
