/* Regression Analysis Lab (Nov. 25th) */

** Salary vs. Education data ;
proc import out=harris 
datafile="D:\�����ڷ� (����, ���п�)\���п�\���п� ���� 2�б�\ȸ�ͺм�(TA)\�ǽ������ڷ�\11�� 25��\HARRIS7.csv"
dbms=csv replace ;
run;

proc print data=harris ;
run ;


* 1) ������ ���� ������ (y=salary, x=education) �׸��� ;
proc sgplot data=harris ;
scatter y=salary x=educat / group=males ;
run ; quit ;


* 2) ���ະ �������ؿ� ���� �޿��� ��Ÿ���� ȸ�ͼ��� ���ú����� �̿��Ͽ� ���϶�. ;
proc reg data=harris ;
model salary=educat males ;
output out=out_harris p=pred ;
run ; quit ;

proc print data=out_harris ;
run ;

proc sgplot data=out_harris ;
scatter y=salary x=educat / group=males ;
series y=pred x=educat / group=males ;
run ;


* 3) �����ڷḸ �̿��ؼ� �������ؿ� ���� �޿��� ��Ÿ���� ȸ�ͼ��� ���϶�. ;
data harris_male ;
set harris ;
if males=0 then delete ;
run ;

proc print data=harris_male ;
run ;

proc reg data=harris_male ;
model salary=educat ;
run ; quit ;


* 4) �����ڷḸ �̿��ؼ� �������ؿ� ���� �޿��� ��Ÿ���� ȸ�ͼ��� ���϶�. ;
data harris_female ;
set harris ;
if males=1 then delete ;
run ;

proc print data=harris_female ;
run ;

proc reg data=harris_female ;
model salary=educat ;
run ; quit ; 


* 5) �������ؿ� ���� �޿���ȭ�� ���࿡ ���� �ٸ��� �����϶�. (Interaction variable) ;
data harris2 ;
set harris ;
edumales=educat*males ;
run ;

proc print data=harris2 ;
run ;

proc reg data=harris2 ;
model salary=educat males edumales ;
run ; quit ; 


* 6) ���� �޿��������� ���ະ �ٸ��ٰ� ���� ���, ���� ���� �������ؿ� ���� �޿��������� ȸ�ͼ��� ���� ���϶�. ;
proc reg data=harris2 ;
model salary=educat males edumales;
output out=out_harris2 p=pred ;
run ; quit ;

proc print data=out_harris2 ;
run ;

proc sgplot data=out_harris2 ;
scatter y=salary x=educat / group=males ;
series y=pred x=educat / group=males ;
run ;
