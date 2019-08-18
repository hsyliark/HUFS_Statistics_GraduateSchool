*** SAS statistics Lab (April.1st) ;


** Producing descriptive statistics ;

* mean, standard deviation, minimum, maximum (������ ������ ����) ;
proc means data=sasuser.survey;
run;

* median, range (������ ������ ����) ;
proc means data=sasuser.survey median range;
run;

* minimum, maximum (������ ������ ����) ;
proc means data=sasuser.diabetes min max;
run;

* minimum, maximum (������ ������ ����, �Ҽ��� ù°�ڸ����� �ݿø�) ;
proc means data=sasuser.diabetes min max maxdec=0; 
run;

* minimum, maximum (������ ������ ����, �Ҽ��� ù°�ڸ����� �ݿø�, Ư�� ������) ;
proc means data=sasuser.diabetes min max maxdec=0;
	var age height weight;
run;

* mean, standard error (������ ������ ����, �Ҽ��� ��°�ڸ����� �ݿø�, Ư�� ������) ;
proc means data=sasuser.survey mean stderr maxdec=2;
	var item1-item5;
run;

* mean, standard deviation, minimum, maximum ;
* �Ҽ��� ��°�ڸ����� �ݿø�, Ư�� ������ ;
* ������ ���ֿ� ���� �����Ͽ� ��� ��Ÿ�� ;
proc means data=sasuser.heart maxdec=1;
	var arterial heart cardiac urinary;
	class survive sex;
run;

* ������ ���� ;
* by ������ ����� �����ϱ� ���ؼ��� sorting�� �ݵ�� �ʿ� ;
proc sort data=sasuser.heart out=work.heartsort;
	by survive sex;
run;
proc means data=work.heartsort maxdec=1;
	var arterial heart cardiac urinary;
	by survive sex;
run;

* work.sum_gender �� ������ ���� ������ ��հ��� �ּڰ��� ���� ;
proc means data=sasuser.diabetes;
	var age height weight;
	class sex;
	output out=work.sum_gender
	mean=AvgAge AvgHeight AvgWeight
	min=MinAge MinHeight MinWeight;
run;
proc print data=work.sum_gender;
run;

* work.sum_gender �� ������ ���� ������ ��հ��� ���� (����� ���� ������� ����) ;
proc summary data=sasuser.diabetes;
	var age height weight;
	class sex;
	output out=work.sum_gender
	mean=AvgAge AvgHeight AvgWeight;
run;
proc print data=work.sum_gender;
run ;


** Producing frequency table ; 

* For each variables ;
proc freq data=sasuser.admit;
	tables sex actlevel;
run;

* Cross tables ;
proc freq data=sasuser.admit;
	tables sex*actlevel;
run;

proc freq data=sasuser.admit;
	tables sex*actlevel*fee;
run;

* ����� list �������� ��� ;
proc freq data=sasuser.admit;
	tables sex*actlevel*fee / list;
run;
