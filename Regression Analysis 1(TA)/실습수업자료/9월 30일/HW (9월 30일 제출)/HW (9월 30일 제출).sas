/* HW : p.91 (6)�� ������ �ѱ����� �ؼ��ϰ�, �ѱ����� ���϶�. */

proc import out=DIV3
datafile="C:\Users\user2\Desktop\2015-2 TA\ȸ�ͺм�(TA)\�ǽ������ڷ�\9�� 23��\DIV3.csv"
dbms=csv replace ;
run ;

proc print data=DIV3 ;
run ;

proc reg data=DIV3 ;
model DIVYIELD=EPS / clb alpha=0.05 ;
plot DIVYIELD*EPS / conf pred ;
run ; quit ;

