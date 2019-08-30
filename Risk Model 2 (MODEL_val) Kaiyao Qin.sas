data MODEL_val1(drop=job reason);
  set MODEL_val;

  JOB_Mgr=(JOB='Mgr'); * dummy variable: when JOB var=Mgr, a new JOB_Mgr variable is created and the value will be =1;
  JOB_Office=(JOB='Office');
  JOB_Other=(JOB='Other');
  JOB_ProfExe=(JOB='ProfExe');
  JOB_Sales=(JOB='Sales');
  JOB_Self=(JOB='Self');
  JOB_miss=(JOB=' ');*missing value;
  REASON_DebtCon=(REASON='DebtCon');
  REASON_HomeImp=(REASON='HomeImp');
  REASON_Miss=(REASON=' ');
run;

proc contents data=MODEL_val1;
run;





/*data cleaning (remove outliers, fill missing values)*/
data MODEL_val11;
  set MODEL_val1;
/*  JOB_Mgr=(JOB='Mgr');
  JOB_Office=(JOB='Office');
  JOB_Other=(JOB='Other');
  JOB_ProfExe=(JOB='ProfExe');
  JOB_Sales=(JOB='Sales');
  JOB_Self=(JOB='Self');
  JOB_miss=(JOB=' ');
  REASON_DebtCon=(REASON='DebtCon');
  REASON_HomeImp=(REASON='HomeImp');
  REASON_Miss=(REASON=' ');*/
  
  if CLAGE=. then CLAGE=95.205;
  if CLAGE>295 then CLAGE=295;
if CLNO=. then CLNO= 42.2; * the order of if statement matters (especially for the same variable;
if CLNO<10 then CLNO=0;
if 10=<CLNO<15 then CLNO= 15;
     DEBTINC_MISS=(DEBTINC=.); *Debtinc_miss is newly created dummy variable (if the observation with DEBTINC=. then Debtinc_miss=1);
  if DELINQ=. then DELINQ=0;
  if DEROG=. then DEROG=0;
  if LOAN>30500 then LOAN=30500;
  if MORTDUE=. then MORTDUE= 46141.88;
  if NINQ=. then NINQ=0;
     VALUE_MISS=(VALUE=.); *value_miss is newly created variable (if the observation with value=. then value_miss=1);;
  if YOJ=. then YOJ=25;
  
run;

PROC CONTENTS DATA=MODEL_val11;
RUN;



/*add logit and probability for (bad is our response variable)*/
/*logit(p)= log(p/1-p)=b0+ b1*x1+ b2*x2+ b3*x3+ b4*x4: the log is natural log (ln)*/
data val2;
  set MODEL_val11;
Logit=
-1.2711	
+2.6732	*	DEBTINC_MISS	/*Debt to income ratio IS MISSING */
-0.5498	*	JOB_Office
+0.7999	*	JOB_Sales
-2.0684	*	JOB_miss
-0.3043*   JOB_ProfExe  
-0.2603*   REASON_DebtCon 	
+4.3884	*	VALUE_MISS		/*Value of current property IS MISSING */
-0.00726*	clage			/*Age of oldest trade line in months*/
+0.6759	*	delinq			/*Number of delinquent trade lines*/
+0.5823	*	derog			/*Number of major derogatory reports*/
+0.1180	*	ninq			/*Number of recent credit inquiries*/
-0.0267	*	yoj				/*Years on current job*/
;
prob=1/(1+exp(-logit)); *exp is e;
run;

proc sort data=val2 out=val21;
   by descending prob;
run;


/*rank and group based on prob*/
proc rank		data = val21
				out = val2_ranked
				groups = 20
				descending; *???prob,rank??;
		var		prob;
		ranks	rank;
run;

data val2_ranked(drop=rank prob);
set val2_ranked;
	model_rank=rank + 1;
	model_score=prob;
run;


/*information about the 20 ranked groups based on prob(model_score) response var (bad) */
ods csv body='ignore this one.csv';
PROC TABULATE DATA = val2_ranked MISSING NOSEPS;
            CLASS model_rank        ;
            VAR   model_score bad  ;
TABLES model_rank='' ALL,      model_score*(MEAN*F=5.3)    bad='BAD'*(sum='# of Bad' n='# of Acct' mean*F=5.3)/box='RANK';
/*box is the top left corner
f=5.3 is format of the mean*/
RUN;
ods csv close;




proc sort data=val2 out=val21;
   by descending prob;
run;

/*rank val1 dataset where P isnt null*/
proc rank data = val21 out = gar2;
 where prob^=.;*^= not equal to;
 var prob;
 ranks rp; 
run;

/*a summary table of counts, number of bad-1,  number of bad=0*/
proc sql;
  select count(*) as tot_obs,
         sum(BAD=1) as resp1,
         sum(BAD=0) as resp0, 
         mean(BAD) as resprate
/*making macro variables*/
  into :tot_obs, :resp1, :resp0, :resprate
  from gar2;
quit;


proc sort data = val21 out=preds21 (keep=prob BAD);
 where prob^=.;
 by descending prob;
run;


                     /*** Lift chart and Moving Avg(Gains Chart)***/
data lft2 (keep=c_resp c_perf c_obs BAD prob t_resp m_avg c_prob avg_resp);
 set preds21;

 if _n_ le 461 then c_perf = _n_ / 461; *for the 1st 728 observations, c perf= observation index/ 728;
 else                  c_perf = 1;

 if BAD = 1 then do; * if bad=1, then...;
   t_resp+1; *start from 0 and add on 1 each iteration (basically the index for bad=1);
   c_resp = t_resp/461; *Cumulative Response= index for bad=1/728;
 end;

 c_obs = _n_ / 2364; *Cumulative observation;

 c_prob + prob; *c_prob =c_prob + prob:(c_prob starts with 0) Cumulative Predicted Prob= prob1+ prob2+ prod3+...+ prodn;
 m_avg=c_prob/_n_; *avg Predicted Prob=Cumulative Predicted Prob/observation index;

 avg_resp = 0.195008;

/*label for the new variables*/
 attrib
        c_resp label = 'Cumulative Response'
        m_avg  label = 'avg Predicted Prob'
        c_prob label = 'Cumulative Predicted Prob'
        c_obs  label = 'Cumulative Population';
run;


proc plot data = lft2 ;                 /*** Lift Chart ***/
 plot (c_resp  c_obs)*c_obs='*' /overlay;* data pts represented by *;
                              
 label c_obs='Cumulative Population'
       c_resp='Cumulative Response';
title "Lift Chart - model";
run;


proc plot data = lft2 ;                 /*** Moving Avg.***/
 plot (m_avg avg_resp )*c_obs='*' / overlay ; 
 format m_avg c_obs avg_resp percent6.;
title "Gains Chart - model";
run;


/*
data hmeq;
   input bad           1
         loan        3-7
         mortdue    9-17
         value     19-27
         reason  $ 29-35
         job     $ 37-42
         yoj       44-49
         derog     51-52
         delinq    54-55
         clage     57-63
         ninq      65-66
         clno      68-69
         debtinc   71-77;
		 /*******
label bad    ="Default or seriously delinquent"
      reason ="Home improvement or debt consolidation"
      job    ="Prof/exec sales mngr office self other"
      loan   ="Amount of current loan request"
      mortdue="Amount due on existing mortgage"
      value  ="Value of current property"
      debtinc="Debt to income ratio"
      yoj    ="Years on current job"
      derog  ="Number of major derogatory reports"
      clno   ="Number of trade (credit) lines"
      delinq ="Number of delinquent trade lines"
      clage  ="Age of oldest trade line in months"
      ninq   ="Number of recent credit inquiries"
		 *********/
      ;


