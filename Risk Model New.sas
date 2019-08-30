libname tt "C:\Users\Jesus kid\Desktop\kqylib";
proc contents data=tt.Hmeq;
run;

/**********
              #    Variable    Type    Len    Label

              1    bad         Num       8    Default or seriously delinquent ??
             10    clage       Num       8    Age of oldest trade line in months
             12    clno        Num       8    Number of trade (credit) lines
             13    debtinc     Num       8    Debt to income ratio
              9    delinq      Num       8    Number of delinquent trade lines
              8    derog       Num       8    Number of major derogatory??? reports
              6    job         Char      6    Prof/exec sales mngr office self other
              2    loan        Num       8    Amount of current loan request
              3    mortdue     Num       8    Amount due on existing mortgage
             11    ninq        Num       8    Number of recent credit inquiries
              5    reason      Char      7    Home improvement or debt consolidation????
              4    value       Num       8    Value of current property
              7    yoj         Num       8    Years on current job

bad
clage
clno
debtinc
delinq
derog
job
loan
mortdue
ninq
reason
value
yoj

***********/


proc freq data=tt.hmeq;
   tables bad reason job;
run;

/*extract random sample*/
data MODEL_DEV MODEL_VAL;* model_dev is the one dataset that will be used;
  set tt.Hmeq;;
  if ranuni(1234567)<=0.6 THEN OUTPUT MODEL_DEV;*designed to generate pseudo random numbers
  on the real line between 0 and 1 with a uniform distribution 
  (FOR EVERY OBSERVATION, if the random number generated is less than 0.6, it goes to MODEL_DEV).;
  ELSE                         OUTPUT MODEL_VAL;
run;


/*create dummy variables for job and reason*/
data MODEL_DEV1(drop=job reason);
  set MODEL_DEV;
  JOB_Mgr=(JOB='Mgr');* dummy variable: when JOB var=Mgr, a new JOB_Mgr variable is created and the value will be =1;
  JOB_Office=(JOB='Office');
  JOB_Other=(JOB='Other');
  JOB_ProfExe=(JOB='ProfExe');
  JOB_Sales=(JOB='Sales');
  JOB_Self=(JOB='Self');
  JOB_miss=(JOB=' ');*missing value;
  REASON_DebtCon=(REASON='DebtCon');
  REASON_HomeImp=(REASON='HomeImp');
  REASON_Miss=(REASON=' ');

/*  if CLAGE=. then CLAGE=179.7662752;
  if CLNO=. then CLNO=21.2960962;
  if DEBTINC=. then DEBTINC=33.7799153;
  if DELINQ=. then DELINQ=0.4494424;
  if DEROG=. then DEROG=0.2545697;
  if LOAN=. then LOAN= 18607.97;
  if MORTDUE=. then MORTDUE=73760.82;
  if NINQ=. then NINQ= 1.1860550;
  if VALUE=. then VALUE=101776.05;
  if YOJ=. then YOJ= 8.9222681;*/

run;

proc contents data=MODEL_DEV1;
run;


/*Macro*/
%let inter_var=
clage
clno
debtinc
delinq
derog
loan
mortdue
ninq
value
yoj
;
%LET DSN=MODEL_DEV1;
%LET RESP=BAD;
%LET GROUPS=10;

%MACRO LOGTCONT                         ;
      OPTIONS CENTER PAGENO=1 DATE;
	  data test;
	    set &DSN; *DSN=MODEL_DEV1;
	  run;

	  %do i=1 %to 10;
	  %LET VBLE=%scan(&inter_var, &i); *, i=1 to 10;

       PROC RANK DATA =TEST (KEEP=&RESP &VBLE)
		/*GROUPS=10 (0:9);*/
			GROUPS = &GROUPS
            OUT = JUNK1&i     ; *OUT = JUNK1&i: if u wanna all 10 datasets (1 for each variable);
            RANKS NEWVBLE         ; *name of the new variable that contains the ranks (not label);
            VAR &VBLE             ; * variable being ranked is yoj;
       RUN                        ;

	
       PROC SUMMARY DATA = JUNK1&i NWAY ;
            CLASS NEWVBLE             ;
            VAR &RESP &VBLE           ;
            OUTPUT OUT = JUNK2&i 
                  MEAN = &RESP &VBLE 
                  MIN(&VBLE)=MIN
                  MAX(&VBLE)=MAX
                     N = NOBS         ;
       RUN                            ;
	   


       DATA JUNK2&i                   ;
            SET JUNK2&i                 ;
            IF &RESP NE 0 THEN
               LOGIT = LOG ( &RESP / (1- &RESP) ) ;
            ELSE IF &RESP = 0 THEN LOGIT = .       ;
       RUN                            ;
	     ;

       PROC SQL NOPRINT;
        CREATE TABLE JUNK3&i AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(&RESP) AS &RESP
        FROM test
        WHERE &VBLE=.
       ;
	   quit;


       DATA JUNK3&i;
        SET JUNK3&i;
        LOGIT=LOG(&RESP/(1-&RESP));
       RUN;

       DATA JUNK4&i;
        SET JUNK2&i JUNK3&i;
       RUN;

        proc plot data=junk4&i;
		PLOT  LOGIT* &VBLE        ;
		plot &resp*&vble;
        plot _freq_*&vble;
		TITLE1 "Plot of Logit(Response) by &VBLE" ;
        TITLE2 "Plot of Response by &VBLE" ;
		TITLE3 "Plot of freq by &VBLE" ;
        run;
	  

       PROC PRINT DATA = JUNK4&i LABEL SPLIT = '*' NOOBS ;
            TITLE "Table of Response by Grouped &VBLE" ;
            VAR NEWVBLE NOBS &VBLE MIN MAX &RESP logit;
            LABEL NEWVBLE = "&VBLE Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
       RUN                                             ;

	   %end;

%MEND LOGTCONT      ;
%LOGTCONT      ;



/*data cleaning (remove outliers, fill missing values)*/
data MODEL_DEV11;
  set MODEL_DEV1;
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

PROC CONTENTS DATA=MODEL_DEV11;
RUN;

/*logistic regression using macro*/
%LET INPUT2=
DEBTINC_MISS
JOB_Mgr
JOB_Office
JOB_Other
JOB_ProfExe
JOB_Sales
JOB_Self
JOB_miss
REASON_DebtCon
REASON_HomeImp
REASON_Miss
VALUE_MISS
clage
clno
delinq
derog
loan
mortdue
ninq
yoj
;

proc logistic data=MODEL_DEV11 descending;
model bad=&input2
  /selection=stepwise fast lackfit rsquare corrb stb;
run;

/*JOB_ProfExe JOB_Sales REASON_DebtCon (loan mortdue)are insignificant variables from the previous log reg*/

%LET INPUT3=
DEBTINC_MISS
JOB_Office
JOB_Other
JOB_Sales
JOB_Self
JOB_miss
REASON_HomeImp
VALUE_MISS
clage
clno
delinq
derog
ninq
yoj
;

proc logistic data=MODEL_DEV11 descending;
model bad=&input3
  /selection=stepwise fast lackfit rsquare corrb stb;
run;

/*selected significant variables*/
%LET INPUT4=
DEBTINC_MISS
JOB_Office
JOB_Sales
JOB_miss
VALUE_MISS
clage
delinq
derog
ninq
yoj
;

proc logistic data=MODEL_DEV11 descending;
model bad=&input4
  /selection=stepwise fast lackfit rsquare corrb stb;
run;


/*add logit and probability for (bad is our response variable)*/
/*logit(p)= log(p/1-p)=b0+ b1*x1+ b2*x2+ b3*x3+ b4*x4: the log is natural log (ln)*/
data val;
  set MODEL_DEV11;
Logit=
-1.5204		
+2.6277		*	DEBTINC_MISS	/*Debt to income ratio IS MISSING */
-0.6524		*	JOB_Office
+1.1876		*	JOB_Sales
-1.933		*	JOB_miss
+0.2527		*	REASON_HomeImp	/*Home improvement */
+4.5662		*	VALUE_MISS		/*Value of current property IS MISSING */
-0.00616	*	clage			/*Age of oldest trade line in months*/
-0.0148		*	clno			/*Number of trade (credit) lines*/
+0.6875		*	delinq			/*Number of delinquent trade lines*/
+0.5215		*	derog			/*Number of major derogatory reports*/
+1.32E-01	*	ninq			/*Number of recent credit inquiries*/
-0.0168		*	yoj				/*Years on current job*/
;
prob=1/(1+exp(-logit)); *exp is e;
run;

proc sort data=val out=val1;
   by descending prob;
run;

/*rank and group based on prob*/
proc rank		data = val1
				out = val_ranked
				groups = 20
				descending; *the larger prob,rank smaller;
		var		prob;
		ranks	rank;
run;

data val_ranked(drop=rank prob);
set val_ranked;
	model_rank=rank + 1;
	model_score=prob;
run;

/*information about the 20 ranked groups based on prob(model_score) response var (bad) */
ods csv body='rank.csv';
PROC TABULATE DATA = val_ranked MISSING NOSEPS;
            CLASS model_rank        ;
            VAR   model_score bad  ;
TABLES model_rank='' ALL,      model_score*(MEAN*F=5.3)    bad='BAD'*(sum='# of Bad' n='# of Acct' mean*F=5.3)/box='RANK';
/*box is the top left corner
f=5.3 is format of the mean*/
RUN;
ods csv close;





proc sort data=val out=val1;
   by descending prob;
run;


%let ds=val1;                   /*output dataset from proc logistic*/
%let response=bad;           /*response variable */
options mprint;
%macro charts(role=);
/*rank val1 dataset where P isnt null*/
proc rank data = &ds out = gar;
 where prob^=.;*^= not equal to;
 var prob;
 ranks rp; 
run;

/*a summary table of counts, number of bad-1,  number of bad=0*/
proc sql;
  select count(*) as tot_obs,
         sum(&response=1) as resp1,
         sum(&response=0) as resp0, 
         mean(&response) as resprate
/*making macro variables*/
  into :tot_obs, :resp1, :resp0, :resprate
  from gar;
quit;


proc sort data = &ds out=preds1 (keep=prob &response);
 where prob^=.;
 by descending prob;
run;

                     /*** Lift chart and Moving Avg(Gains Chart)***/

data lft (keep=c_resp c_perf c_obs &response prob t_resp m_avg c_prob avg_resp);
 set preds1;

 if _n_ le &resp1 then c_perf = _n_ / &resp1; *for the 1st 728 observations, c perf= observation index/ 728;
 else                  c_perf = 1;

 if &response = 1 then do; * if bad=1, then...;
   t_resp+1; *start from 0 and add on 1 each iteration (basically the index for bad=1);
   c_resp = t_resp/&resp1; *Cumulative Response= index for bad=1/728;
 end;

 c_obs = _n_ / &tot_obs; *Cumulative observation;

 c_prob + prob; *c_prob =c_prob + prob:(c_prob starts with 0) Cumulative Predicted Prob= prob1+ prob2+ prod3+...+ prodn;
 m_avg=c_prob/_n_; *avg Predicted Prob=Cumulative Predicted Prob/observation index;

 avg_resp = &resprate;

/*label for the new variables*/
 attrib
        c_resp label = 'Cumulative Response'
        m_avg  label = 'avg Predicted Prob'
        c_prob label = 'Cumulative Predicted Prob'
        c_obs  label = 'Cumulative Population';
run;


proc plot data = lft ;                 /*** Lift Chart ***/
 plot (c_resp  c_obs)*c_obs='*' /overlay;* data pts represented by *;
                              
 label c_obs='Cumulative Population'
       c_resp='Cumulative Response';
title "Lift Chart - &role";
run;

proc plot data = lft ;                 /*** Moving Avg.***/
 plot (m_avg avg_resp )*c_obs='*' / overlay ; 
 format m_avg c_obs avg_resp percent6.;
title "Gains Chart - &role";
run;


%mend;
%charts(role=model)


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


