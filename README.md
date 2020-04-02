# README

---

[1. About this repo](#repo)

[2. Overview](#overview)

[3. Analysis](#analysis)

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

Similar analysis for were conducted to assess *ED Literacy Scores*(measured through a knowledge test), *ED Stigma Scores* (measured through an attitudes and knowledge test), and *Openness to ED Education* (measured through a self reported attitudes). These are not included in the notebook, but are in the R script. (More about this in the conclusions section.)

#### Data

The dataset consists survey data (n=1000) collected over 1 month in the summer of 2018. Respondents who were domain experts in eating disorders were excluded. Data was collected in Canada, from all provinces, and the dataset was well balanced (age/geographic location). Occupational category sizes were not balanced, but variances accross occupational categories were similar (normality, homogeneity tests suggested ANOVA was appropriate.)

## <a name="analysis">Analysis</a>

#### Setup

I compare (self-assessed) **eating disorder knowledge** accross different **occupational categories**.

The dependent variable, **RSR_Score**, eating disorder knowledge was created using three survey questions:
- Identify_Recognize: ‘I believe I can recognize the signs of an eating disorder in others’
- Identify_Support: ‘I feel prepared to support someone who is experience an eating disorder’
- Identify_Resources: 'I know about the resources available to support people experiencing eating disorders’

The overall RSR scores can be observed in the following figure:

RSR scores:

<img src="https://github.com/iurrutia/MHLiteracySurvey/blob/master/images/rsrhist.png" width="400">


The following table shows RSR scores by occupation:

<img src="https://github.com/iurrutia/MHLiteracySurvey/blob/master/images/rsrbyocc.png" width="400">


And the following figure shows the RSR scores per occupational category:

<img src="https://github.com/iurrutia/MHLiteracySurvey/blob/master/images/rsrocchist.png" width="400">



#### Test

We use an ANOVA to test the hypothesis that the mean RSR score is the same accross occupational categories, and we reject this hypothesis with a p-value of 5.27e-09.

This is helpful, but we also want to know which occupational categories should be recommended. We perform Tukey's HSD to examine differences between pairs of categories to see which categories have higher/lower RSR scores. Tukey's HSD allows us to perform multiple comparisons, adjusting for 95% family-wise confidence level.

Confidence intervals for significance between pairs of occupational categories (Tukey's HSD):

<img src="https://github.com/iurrutia/MHLiteracySurvey/blob/master/images/testb.jpg" width="100">

We use CLD to create buckets of categories to separate out occupations with high/low RSR scores:

<img src="https://github.com/iurrutia/MHLiteracySurvey/blob/master/images/CLD.png" width="400">


## <a name="concl">Conclusions and next steps</a>

We separate out occupational categories into ones with high RSR score, low RSR score, and 'neither'. 

This is an example with ANOVA for one variable, but the analysis was actually conducted as a MANOVA for dependent variables:

- Stigma Score: Attitudes and beliefs which were seen as stigmatizing ED
- Literacy Score: An ED literacy score, quiz-score based
- Openness to ED education: ED-related education education, openness to and work towards receiving more ED-related education
- RSR score: Preparedness to recognize, support, and provide resources to individuals who may be experiencing EDs

