/*** (2.25) Gas Mileage (highway) for 147 cars  : 
          - �ڵ��� �𵨰�� Best 5 models ? /  Worst 5 models ?
          -(histogram, univariate, t-test in sas ����ؼ� �ڷ� �����ϱ�)  
+ C.I. ������ ����Ͽ� sas �� ���ϱ� ***/

/** p31 ex 2-25 **/

proc import out=cars2
datafile='C:\Users\user\Desktop\CARS2.csv' 
dbms=csv replace ;
run ;

/* Best 5 model, Worst 5 model */

proc sort data=cars2 ;
by HWYMPG ;
run ;

proc print data=cars2 ;
run ;

/* Explanation data */

proc univariate data=cars2 ;
var HWYMPG ;
histogram / outhistogram=outhisto cfill=green 
vaxis=0 to 50 by 5
midpoints=10 to 70 by 5 ;
run ;
quit ;

proc ttest data=cars2 ;
var HWYMPG ;
run ;


/*** (2.30) CAFE (corporate average fuel economy ) standard �� �� �ʿ����� ���϶� 
          - CAFE ������ 27.5 ��� ǥ������ ���õ� 147�� �ڵ��� ��� �̿� ��ġ�� ���ϴ� �ڵ����� 
��ü ǥ���� ��%���� ���϶�.
          - 2003�� ����� ��� �ڵ������� ��� gas mileage �� CAFE ������  27.5 �� ����ϴ��� ���θ� 
���Ǽ��� 0.05�� ���������Ͽ��� ***/

/** p39 ex 2-30 **/

data cars2_1 ;
set cars2 ;
if HWYMPG < 27.5 then delete ;
run ;

proc print data=cars2_1 ;
run ;

/* Hypothesis testing */

proc ttest data=cars2 H0=27.5 ;
var HWYMPG ;
run ;

/*** (2.32)  GMAT ��պ�  ������ ����ϰ� sas �� Ȯ���ϱ� (F-test ����) ***/

/** p43 ex 2-32 **/

/* F-test for testing difference of variation */

data gmat1 ;
xbar1=545 ; xbar2=510 ; n1=50 ; n2=50 ;
s1=104 ; s2=95 ; f=s1**2/s2**2 ;
f1=finv(0.95,49,49) ;
s_prob=1-probf(f,49,49) ;
run ;

proc print data=gmat1 ;
run ;

/* Confidence interval for difference between mean GMAT */

data gmat2 ;
xbar1=545 ; xbar2=510 ; n1=50 ; n2=50 ;
s1=104 ; s2=95 ;
df=n1+n2-2 ;
t=tinv(0.975,df) ;
sp2=((n1-1)*s1**2+(n2-1)*s2**2)/(n1+n2-2) ;
ci_lower=(xbar1-xbar2)-t*sqrt(sp2*(1/n1+1/n2)) ; 
ci_high=(xbar1-xbar2)+t*sqrt(sp2*(1/n1+1/n2)) ;
run ;

proc print data=gmat2 ;
run ; 






 







