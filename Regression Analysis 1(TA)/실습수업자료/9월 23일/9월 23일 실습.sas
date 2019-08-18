/** 9�� 23�� ȸ�ͺм� �ǽ� **/

/* p74. exercise 3 */


* 1. p.74 (3)�� ���� 3���� �ڷḦ ���� Scatter Plot �Ͻÿ�. ;

proc import out=REALEST3
datafile="C:\Users\0\Desktop\ȸ�� 9�� 23��\REALEST3.csv"
dbms=csv replace ;
run ;

proc import out=COMNODE3
datafile="C:\Users\0\Desktop\ȸ�� 9�� 23��\COMNODE3.csv"
dbms=csv replace ;
run ;

proc import out=HSTARTS3
datafile="C:\Users\0\Desktop\ȸ�� 9�� 23��\HSTARTS3.csv"
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



symbol1  V=circle C=blue  I= r ;

proc gplot data=REALEST3 ;
plot VALUE*SIZE ;
run ; quit ;

proc gplot data=COMNODE3 ;
plot COST*NUMPORTS ;
run ; quit ;

proc gplot data=HSTARTS3 ;
plot STARTS*RATES ;
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





/* HW : p.91 (6)�� ������ �ѱ����� �ؼ��ϰ�, �ѱ����� ���϶�. */


