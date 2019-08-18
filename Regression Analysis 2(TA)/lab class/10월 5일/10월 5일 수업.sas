/** p74. exercise 3.3 **/

* 1. p.74 (3)�� ���� 3���� �ڷḦ ���� Scatter Plot �Ͻÿ�. ;

proc import out=REALEST3
datafile="C:\Users\0\Desktop\10�� 5�� ����\REALEST3.csv"
dbms=csv replace ;
run ;

proc import out=COMNODE3
datafile="C:\Users\0\Desktop\10�� 5�� ����\COMNODE3.csv"
dbms=csv replace ;
run ;

proc import out=HSTARTS3
datafile="C:\Users\0\Desktop\10�� 5�� ����\HSTARTS3.csv"
dbms=csv replace ;
run ;

proc print data=REALEST3 ;
run ;

proc print data=COMNODE3 ;
run ;

proc print data=HSTARTS3 ;
run ;

proc sgplot data=REALEST3 ;
scatter x=SIZE y=VALUE ;
title "REALEST3" ;
run ; quit ;

proc sgplot data=COMNODE3 ;
scatter x=NUMPORTS y=COST ;
title "COMNODE3" ;
run ; quit ;

proc sgplot data=HSTARTS3 ;
scatter x=RATES y=STARTS ;
title "HSTARTS3" ;
run ; quit ;

* 2. Regression line �� ���� ���϶�. ;

* 3. ���⿡ ���� 95% �ŷڱ����� sas output �� ���� ���϶�. ;

proc reg data=REALEST3 ;
model VALUE=SIZE / clb alpha=0.05 ;
plot VALUE*SIZE / conf pred ;
run ; quit ;

proc reg data=COMNODE3 ;
model COST=NUMPORTS / clb alpha=0.05 ;
plot COST*NUMPORTS / conf pred ;
run ; quit ;

proc reg data=HSTARTS3 ;
model STARTS=RATES / clb alpha=0.05 ;
plot STARTS*RATES / conf pred ;
run ; quit ;

data confidence ;
t1=tinv(0.975,98) ; t2=tinv(0.975,12) ; t3=tinv(0.975,38) ; 
run ;

proc print data=confidence ;
run ;



