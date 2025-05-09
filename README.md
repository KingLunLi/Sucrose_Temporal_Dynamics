# Overview
This repository contains all the custom and adapted codes (Python, R, and Arduino) used to analyze and visualize our calcium imaging and behavioral data for the manuscript titled *Temporal dynamics of nucleus accumbens neurons in male mice during reward seeking* [(Schall TA, Li KL, Qi X, et al. Nat.Commun. 2024)](https://www.nature.com/articles/s41467-024-53690-8).

## Summary 
This study investigates how the nucleus accumbens (NAc) regulates reward-motivated behavior by analyzing the temporal dynamics of NAc neurons in mice performing self-paced lever-presses for sucrose. We recorded Ca²⁺ activity in individual NAc neurons and identified distinct patterns of neuronal activation at specific time points before lever-pressing. Our findings suggest that the time-specific activity of D1- and D2-neurons plays a crucial role in initiating reward-seeking behavior, highlighting the temporal organization of motivation-driven actions in the NAc.

## Methods
**Miniscope**

<img src="/images/Miniscope%201.png">
This study utilized GCaMP-based in vivo Ca²⁺ imaging to monitor neuronal activity in mice, using GCaMP6m in wild-type mice and Flex.GCaMP6m in D1-Cre or D2-Cre mice. Viral injections were followed by GRIN lens implantation above the injection site. For optogenetic manipulations, Jaws-expressing AAV was used for in vivo expression.
<br><br>

**Sample *in-vivo* calcium imaging recording (Press *Play* to view the recording)**

<img src="/images/Ca2+Imaging.gif" width="25%" height="25%">
Sucrose self-administration (SA) was conducted in operant chambers equipped with active and inactive levers, conditioned stimulus lights, and audio cues. Neuronal activity during sucrose SA was recorded using the wire-free UCLA Miniscope (v3), enabling precise monitoring of NAc neuronal dynamics during reward-seeking behavior.<br><br>

**Extracted Ca2+ signal**

<img src="/images/Extracted Traces.png" width="25%" height="25%">
Miniscope videos of Ca²⁺ activity were recorded at ~320 × 320 µm resolution with a 20 Hz framerate and processed using the Ca²⁺ imaging analysis (CalmAn) package in Python. Processing included motion correction, source extraction, and deconvolution to obtain fluorescence traces of Ca²⁺ activity. To normalize signals across trials, extracted Ca²⁺ traces were binned into 100-ms segments and z-score standardized (mean = 0, standard deviation = 1). For manifold analysis, ΔF/F₀ was used, where F₀ was the mean z-score over a 5-minute sliding baseline, and ΔF represented the difference between fluorescence intensity at time t and F₀.<br><br><br><br>

## Analysis

**Sorted data**

<img src="/images/Sorted data.png" width="25%" height="25%">

**PCA**

<img src="/images/PCA.png" width="25%" height="25%">

**SVM accuracy**

<img src="/images/Accuracy.png" width="25%" height="25%">

**Non-linear dimenality reduction Trajectories**

<img src="/images/Trajectory.png" width="25%" height="25%">

**DLC labeling**

<img src="/images/DLC labeling.png" width="25%" height="25%">

**DLC Prediction**

<img src="/images/DLC Prediction Accuracy.png" width="25%" height="25%">

**Optogenetics**

<img src="/images/Opto.png" width="25%" height="25%">

**Optogenetics self-administration training**

<img src="/images/Opto training results.png" width="25%" height="25%">



