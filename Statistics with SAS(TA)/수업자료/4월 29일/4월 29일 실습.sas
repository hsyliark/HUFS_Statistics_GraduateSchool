*** SAS Statistics Lab (April.29th) ;


** ���ϴ� �κи� �����ؼ� ������ merging (one-to-one) ;
* ��ġ�� �ʴ� ����ġ�� ������ ;
data work.one2one;
set sasuser.patients;
if age<60;
set sasuser.measure;
run;
proc print data=work.one2one;
run;


** ������ setting ;

* Ư�� ���� ���� ;
data work.concat;
set sasuser.therapy1999 sasuser.therapy2000;
run;
proc print data=work.concat;
run;

* Ư�� ������ �������� ���� ;
data work.interlv;
set sasuser.therapy1999 sasuser.therapy2000;
by month;
run;
proc print data=work.interlv;
run ;


** Ư�� ���� �������� ���� �� ������ merging ; 
proc sort data=sasuser.demog out=work.demog1;
by id;
run;
proc print data=work.demog1;
run;
proc sort data=sasuser.visit out=work.visit1;
by id;
run;
proc print data=work.visit1;
run;

data work.merged;
merge work.demog1 work.visit1;
by id;
run;
proc print data=work.merged;
run;

* merging �� ���� date�� ���� overwriting ���� ;
* rename ���� ;
data work.merged1;
merge work.demog1(rename=(date=BirthDate))
work.visit1(rename=(date=VisitDate));
by id;
run;
proc print data=work.merged1;
run;
