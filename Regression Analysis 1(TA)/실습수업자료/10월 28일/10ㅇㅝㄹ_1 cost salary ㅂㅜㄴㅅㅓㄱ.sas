/*148�� ��������1��+158�ʿ�������3��*/
proc import
out=cost
datafile='H:\ȸ�ͺм�\cost4.csv'
dbms=csv replace ;
run;
/*full model*/
proc reg data=cost;
model cost = paper machine overhead labor ;
run;quit;
/*reduced model*/
proc reg data=cost;
model cost = paper machine ;
run;quit;


/*150�� �������� 2��+158�ʿ�������4��*/
proc import
out=aaa
datafile='H:\ȸ�ͺм�\harris4.csv'
dbms=csv replace ;
run;

proc reg data=aaa;
model salary = educ exper time;
run;quit;

proc reg data=aaa;
model salary = educ ;
run;quit;

