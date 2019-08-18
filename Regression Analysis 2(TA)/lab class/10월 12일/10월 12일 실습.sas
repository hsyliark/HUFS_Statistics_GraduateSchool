/* Oct. 12th Lab class */

* Question 1~7 ;

proc import out=COMNODE3
datafile="C:\Users\user2\Desktop\ȸ�ͺм�(TA)\10�� 12��\COMNODE3.csv"
dbms=csv replace ;
run ;

proc print data=COMNODE3 ;
run ;

* 1) Numports �ڷḦ �̿��ؼ� proc reg �� beta_0, beta_1 , sigma^2 ����ġ ���Ѵ�. ;

proc reg data=COMNODE3 ;
model cost=numports / clb alpha=0.05 ;
run ; quit ;

* 2)  sum_i^n (x_i - x_bar)^2 ��  sas �� �̿��� ���Ѵ�. ( proc univariate ) ;

proc univariate data=COMNODE3 ;
var numports ;
run ; quit ;

* 3) S.E.(\hat\mu_Y|x=40)  , S.E.(\hat\mu_Y|x=68) �� 1), 2) �� ����� �̿��� ������ ���Ѵ�. ;
* 4) 3)�� �̿��Ͽ�  port ���� 40, 68�� ��쿡 95% C.I., 95% P.I. �� ������ ���Ѵ�. ;
* 5) �� 4)�� �����  sas �� ���� ���� C.I.,  P.I. �� ���Ѵ�. ;

data COMNODE3_1 ;
input COST NUMPORTS ; cards ;
. 40
. 68
;
run ;

data total ; set COMNODE3 COMNODE3_1 ;
run ;

proc print data=total ;
run ;

proc reg data=total;
model cost=numports ;
output out=predict p=y_pred LCL=cost_95LCL UCL=cost_95UCL LCLM=ecost_95LCL UCLM=ecost_95UCL ;
run ; quit ;

proc print data=predict(where=(cost=.)) noobs ;
run ;

* 6) Numports �ڷῡ�� R^2 �� ���ϰ�, Corr(y, y_hat),  Corr(y, x) �� ���� ���ؼ� R^2 ���� ���踦 Ȯ���϶�. ;

proc reg data=COMNODE3 ;
model cost=numports / clb alpha=0.05 ;
output out=result p=y_hat ;
run ; quit ;

proc print data=result ;
run ;

proc corr data=result ;
var COST NUMPORTS y_hat ;
run ; quit ;
  
* 7)  ���� p.119  #21  Fill the blanks in ANOVA table. (See text book.) ;



* Question 8~9 ;

proc import out=ABXSALES3
datafile="C:\Users\user2\Desktop\ȸ�ͺм�(TA)\10�� 12��\ABXSALES3.csv"
dbms=csv replace ;
run ;

data ABXSALES3 ; set ABXSALES3 ;
time+1 ;
run ;

proc print data=ABXSALES3 ;
run ;

* 8)  ABX company sales (Time Linear Trend )  �� �̿��ؼ� (x,y) ������ �׸���. (���� ���� ���� ������  ǥ���� �� -proc sgplot �� series �ɼ�) ;

proc sgplot data=ABXSALES3 ;
scatter x=time y=SALES ;
series x=time y=SALES ;
run ; quit ;

* 9)  �� �׷����� Linear Trend regression line �� overlay ���Ѻ��ƶ�. ;

proc reg data=ABXSALES3 ;
model SALES=time ;
output out=res_sales p=sales_hat ;
run ; quit ;

proc print data=res_sales ;
run ;

proc sgplot data=res_sales ;
scatter x=time y=SALES ;
series x=time y=SALES ;
series x=time y=sales_hat ;
run ; quit ;




 








