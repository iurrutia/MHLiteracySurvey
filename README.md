# README

---

[1. About this repo](#repo)

[2. Overview](#overview)

[3. Model](#model)

[4. Conclusions and next steps](#concl)


---

## <a name="repo">About this repo</a>

### Context for this repo:

This repo contains materials relating to some of the analysis done on NEDIC's mental health literacy survey.

Pre-processing and cleaning scripts are not included in this repo. 

### What is in this repo:

- Final script used to create report for NEDIC: [R script](https://github.com/iurrutia/MHLiteracySurvey/blob/master/CEDAnalysis2019.R)  

- A part of the script was pasted into a notebook to present a portion of this analysis to show an example of the analyses perfomed using the R script: [Notebook for Insight](https://github.com/iurrutia/MHLiteracySurvey/blob/master/CEDAnalysis2019.R) 

- Other inclusions are older/preliminary versions of the analysis

This README explains the analysis in the notebook for Insight. 

## <a name="overview">Overview</a>

#### Goal

The [notebook](https://github.com/iurrutia/MHLiteracySurvey/blob/master/CEDAnalysis2019.R) illustrates part of an analysis of survey data collected for an eating disorder organization. Among other things, this organization  develops and delivers educational material (e.g. workshops, webinars, and other in-person and online projects) for different audiences.
This notebook illustrates the process of applying survey data to help stakeholders answer questions around deciding where to focus this energy. (E.g. What occupational sectors would be good areas to apply future resources to creating/delivering educational resources? Teachers, or fitness professionals, etc.)

Similar analysis for were conducted to assess *ED Literacy Scores*(measured through a knowledge test), *ED Stigma Scores* (measured through an attitudes and knowledge test), and *Openness to ED Education* (measured through a self reported attitudes). These are not included in the notebook, but are in the R script.

#### Data

The dataset consists survey data (n=1000) collected over 1 month in the summer of 2018. Respondents who were domain experts in eating disorders were excluded. Data was collected in Canada, from all provinces, and the dataset was well balanced (age/geographic location). Occupational category sizes were not balanced, but variances accross occupational categories were similar (normality, homogeneity tests suggested ANOVA was appropriate.)


#### How

I compare (self-assessed) **eating disorder knowledge** accross different **occupational categories**.

The dependent variable, **RSR_Score**, eating disorder knowledge was created using three survey questions:
- Identify_Recognize: ‘I believe I can recognize the signs of an eating disorder in others’
- Identify_Support: ‘I feel prepared to support someone who is experience an eating disorder’
- Identify_Resources: 'I know about the resources available to support people experiencing eating disorders’

The overall RSR scores can be observed in the following figure:


RSR scores:
<img src="https://github.com/iurrutia/MiniMLProjects/blob/master/images/rsrhist.jpg" width="400">


The following table shows RSR scores by occupation:
<img src="https://github.com/iurrutia/MiniMLProjects/blob/master/images/rsrbyocc.png" width="400">


rsrocchist



Confidence intervals for significance between pairs of occupational categories (Tukey's HSD)
<img src="https://github.com/iurrutia/MiniMLProjects/blob/master/images/testb.jpg" width="400">


