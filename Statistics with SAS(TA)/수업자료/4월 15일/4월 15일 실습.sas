*** SAS statistics Lab (April.15th) ;



** ������ ������ ������ �б� ;
* @1, @9, @19 -> ������ ���ڿ� �ش��ϴ� �ٺ��� ���ο� ������ �б� ;
* $5., $7. -> ���ں����� �ִ���� ���� ;
* +7 -> FirstName ������ ���� �� �����͸� ������ �������� 7ĭ �̵���Ŵ ; 
* comma9. -> ','ǥ�� �����ϰ� ���ں����� �б� ;

filename empdata 'C:\Users\user\Desktop\practice\empdata.txt' ;

data empinfo;
	infile empdata;
	input @9 FirstName $5. @1 LastName $7. +7 JobTitle 3.
	@19 Salary comma9.;
run;
proc print data=empinfo;
run;



** �����ڷ� �и��� ������ �б� ;
* 1 �����ڸ� ���� �б� -> ' ' ;

filename credit 'C:\Users\user\Desktop\practice\credit1.txt' ;

data survey;
	infile credit;
	input Gender $ Age Bankcard FreqBank Deptcard FreqDept;
run;
proc print data=survey ;
run ;


* 2 �����ڸ� ���� �б� -> ',' ;

filename credit 'C:\Users\user\Desktop\practice\credit2.txt' ;

data survey;
	infile credit dlm=',';
	input Gender $ Age Bankcard FreqBank Deptcard FreqDept;
run;
proc print data=survey ;
run ;



** ������ ������ ������ �б� ;
* @13, @21 -> ������ ���ڿ� �ش��ϴ� �ٺ��� ���ο� ������ �б� ;
* date7. -> ��¥������ ������ ���ڷ� ��ȯ ;
* comma6. -> ','ǥ�ø� ���ְ� ���ں����� �б� ;
* $9. -> ���ں����� �ִ���� ���� ; 

filename rawdata 'C:\Users\user\Desktop\practice\rawdata.txt' ;

data mixed;
	infile rawdata;
	input SSN $ 1-11 @13 HireDate date7. @21 Salary comma6.
	Department : $9. Phone;
run;
proc print data=mixed;
run;
