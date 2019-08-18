proc import 
 datafile='I:\ȸ�ͺм��ǽ�\�ǽ� �ڷ�\harris7.csv'
 out=salary
 dbms=csv replace;
run;


proc sgplot data=salary;
 scatter y=salary x=educat/ group=males;
run;

proc gplot data= salary ;
plot salary * educat ;
by males;
run;

*���ະ �������ؿ� ���� �޿��� ��Ÿ���� ȸ�ͼ��� ���ϰ� ������ ȸ�ͼ�;
proc reg data=salary;
 model salary=educat males;
 output out=out  p=predic;
run;quit;

proc sgplot data=out;
 scatter y=salary x=educat / group=males;
 series y=predic x=educat / group=males;
run;

*���ະ �������ؿ� ���� �޿����������� ������ �ٸ��� ���� ��ȣ�ۿ� ���� ����;
data salary2;
 set salary;
 edumales=educat*males;
run;

*�޿����������� �ٸ��ٰ� ���� ���, ���� ���� �������ؿ� ���� �޿������� ��Ÿ���� ȸ�ͼ�;
proc reg data=salary2;
 model salary=educat males edumales;
 output out=out2 p=predic;
run;quit;

proc sgplot data=out2;
 scatter y=salary x=educat / group=males;
 series y=predic x=educat / group=males;
run;
