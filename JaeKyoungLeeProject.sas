*Descriptive Statistics;
proc print data=heartfail;
run;

*Continuous variables;
proc univariate data=heartfail;  
	class Death;
	var Death age;
	histogram;
run;

proc univariate data=heartfail;  
	class Death;
	var Death CPK;
	histogram;
run;

proc univariate data=heartfail;  
	class Death;
	var Death Efrac;
	histogram;
run;


proc univariate data=heartfail;  
	class Death;
	var Death plat;
	histogram;
run;

proc univariate data=heartfail;  
	class Death;
	var Death sercr;
	histogram;
run;

proc univariate data=heartfail;  
	class Death;
	var Death serso;
	histogram;
run;

proc univariate data=heartfail;  
	class Death;
	var Death time;
	histogram;
run;




*Association;
*Binary Var = anae, diab, hbp, sex, smk; 
proc freq data=heartfail order = data;
	table Death*anae/norow nocol nopercent expected chisq;
run;

proc freq data=heartfail order = data;
	table Death*diab/norow nocol nopercent expected chisq;
run;

proc freq data=heartfail order = data;
	table Death*hbp/norow nocol nopercent expected chisq;
run;

proc freq data=heartfail order = data;
	table Death*sex/norow nocol nopercent expected chisq;
run;

proc freq data=heartfail order = data;
	table Death*smk/norow nocol nopercent expected chisq;
run;


*Goodness of fit tests for continuous predictors;
*(age CPK Efrac plat sercr serso time) ;
proc sort data=heartfail;
  by Death;
run;

proc univariate data=heartfail normal;  
	var age;
	histogram age/ normal;
	probplot age;
	ods select TestsForNormality Histogram ProbPlot;
run;

proc univariate data=heartfail normal;  
	var CPK;
	histogram CPK/ normal;
	probplot CPK;
	ods select TestsForNormality Histogram ProbPlot;
run;

proc univariate data=heartfail normal;  
	var Efrac;
	histogram Efrac/ normal;
	probplot Efrac;
	ods select TestsForNormality Histogram ProbPlot;
run;

proc univariate data=heartfail normal;  
	var plat;
	histogram plat/ normal;
	probplot plat;
	ods select TestsForNormality Histogram ProbPlot;
run;

proc univariate data=heartfail normal;  
	var sercr;
	histogram sercr/ normal;
	probplot sercr;
	ods select TestsForNormality Histogram ProbPlot;
run;

proc univariate data=heartfail normal;  
	var serso;
	histogram serso/ normal;
	probplot serso;
	ods select TestsForNormality Histogram ProbPlot;
run;

proc univariate data=heartfail normal;  
	var time;
	histogram time/ normal;
	probplot time;
	ods select TestsForNormality Histogram ProbPlot;
run;

*comparing mean values;
proc npar1way data=heartfail wilcoxon;
  class Death;
  var age;
  ods exclude KruskalWallisTest;
run;

proc npar1way data=heartfail wilcoxon;
  class Death;
  var CPK;
  ods exclude KruskalWallisTest;
run;

proc npar1way data=heartfail wilcoxon;
  class Death;
  var Efrac;
  ods exclude KruskalWallisTest;
run;

proc npar1way data=heartfail wilcoxon;
  class Death;
  var plat;
  ods exclude KruskalWallisTest;
run;

proc npar1way data=heartfail wilcoxon;
  class Death;
  var sercr;
  ods exclude KruskalWallisTest;
run;

proc npar1way data=heartfail wilcoxon;
  class Death;
  var serso;
  ods exclude KruskalWallisTest;
run;

proc npar1way data=heartfail wilcoxon;
  class Death;
  var time;
  ods exclude KruskalWallisTest;
run;




*fitting logistic model;
proc logistic data=heartfail desc;
	class anae diab hbp sex smk /param=ref;
	model Death = age--smk;
	ods select OddsRatios ParameterEstimates 
		GlobalTests ModelInfo FitStatistics;
run;

proc logistic data=heartfail desc;
	class anae diab hbp sex smk /param=ref;
	model Death = age--smk/selection=stepwise;
	ods select OddsRatios ParameterEstimates 
		GlobalTests ModelInfo FitStatistics ModelBuildingSummary;
run;

proc logistic data=heartfail desc;
	model Death = age Efrac sercr;
	ods select OddsRatios ParameterEstimates 
		GlobalTests ModelInfo FitStatistics;
run;

*logistic model with stratification time;
proc logistic data=heartfail desc;
	class anae diab hbp sex smk/param=ref;
	model Death = age--smk/selection=stepwise;
	strata month;
	ods select OddsRatios ParameterEstimates 
		GlobalTests ModelInfo FitStatistics ModelBuildingSummary;
run;

proc logistic data=heartfail desc;
	class anae diab hbp sex smk/param=ref;
	model Death = age Efrac sercr;
	strata month;
	ods select OddsRatios ParameterEstimates 
		GlobalTests ModelInfo FitStatistics;
run;

*Predicting probabilites and table(logit);
proc logistic data=heartfail desc;
	model Death = age Efrac sercr;
    output predicted=probs out=pred;
run;

data pred;
	set pred;
	if probs > 0.4 then predicted_L = 1;
	if probs < 0.4 then predicted_L = 0;
run;

proc print data=pred; 
run;

proc freq data=pred;
    tables Death*predicted_L/nopercent norow nocol;
run;

*Predicting probabilites and table(Conditional logit);
proc logistic data=heartfail desc;
	model Death = age Efrac sercr month;
	*strata month;
    output predicted=pred out=predictions;
run;

data predictions;
	set predictions;
	if pred > 0.4 then predicted_L = 1;
	if pred < 0.4 then predicted_L = 0;
run;

proc sort data=predictions;
	by Death;
run;

proc print data=predictions; 
run;

* compare levels observed with levels predicted;
proc freq data=predictions;
    tables Death*predicted_L/nopercent norow nocol;
run;


*Discriminant Analysis;

proc stepdisc data=heartfail sle=.05 sls=.05;
   	class Death;
   	var age CPK Efrac plat sercr serso time month;
	ods select Summary;
run;

proc discrim data=heartfail pool=test
	manova list simple wcov crossvalidate;
	class Death;
	var age Efrac sercr month;
	priors proportional;
	ods select ChiSq MultStat ClassifiedCrossVal ErrorCrossVal;
run;



proc univariate data = misclass;
	class Death;
	var age Efrac sercr month;
	histogram;
run;


*Oversampling method;
data have;
	set heartfail;
run;

proc sort data=have;
	by Death;
run;

proc surveyselect data=have out=want method=urs sampsize=(203 203)  outhits;
	strata Death;
run;

proc sort data = want;
	by Death;
run;
proc print data=want;
run;

*Exploring observations that were misclassified;
data misclass;
	set predictions;
	where predicted_L not = Death;
run;

data accurate;
	set predictions;
	where predicted_L = Death;
run;

proc print data=misclass;
run;

proc print data=accurate;
run;



proc print data=want;
run;


proc logistic data=want desc;
	model Death = age Efrac sercr;
	strata month;
    output predicted=pred out=predictions;
run;

data predictions;
	set predictions;
	if pred > 0.4 then predicted_L = 1;
	if pred < 0.4 then predicted_L = 0;
run;

* compare levels observed with levels predicted;
proc freq data=predictions;
    tables Death*predicted_L/nopercent norow nocol;
run;

*Discriminant for oversampled data;
proc stepdisc data=want sle=.05 sls=.05;
   	class Death;
   	var age CPK Efrac plat sercr serso month;
	ods select Summary;
run;

*comparing misclass dataset and accurate dataset;
*significant variables;
proc univariate data=misclass;  
	var age;
	histogram;
run;

proc univariate data=accurate;  
	var age;
	histogram;
run;

proc univariate data=misclass;  
	var Efrac;
	histogram;
run;

proc univariate data=accurate;  
	var Efrac;
	histogram;
run;

proc univariate data=misclass;  
	var sercr;
	histogram;
run;

proc univariate data=accurate;  
	var sercr;
	histogram;
run;

proc univariate data=misclass;  
	var month;
	histogram;
run;

proc univariate data=accurate;  
	var month;
	histogram;
run;












