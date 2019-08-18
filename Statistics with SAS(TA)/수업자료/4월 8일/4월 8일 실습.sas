*** SAS statistics Lab (April.8th) ;



** ���� ���� ��� ;


* ������ �ҷ����̱� ;
filename tests 'C:\Users\user2\Desktop\Apr.8th\stress.dat';


* �ش� ������ �ҷ����̱� ;
* ����TotalTime�� ���� ���� ���ϱ� (���� SumSec) ;
data stress;
	infile tests;
	input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
	RecHR 35-37 TimeMin 39-40 TimeSec 42-43
	Tolerance $ 45;
	TotalTime=(timemin*60)+timesec;
	SumSec+totaltime;
run;


* �ش� ������ �ҷ����̱� ;
* ����TotalTime�� ���� ���� ���ϱ� (���� SumSec�� 5400���� �����ؼ� ���ذ��� ������ ����) ;
data stress;
	infile tests;
	input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
	RecHR 35-37 TimeMin 39-40 TimeSec 42-43
	Tolerance $ 45;
	TotalTime=(timemin*60)+timesec;
	retain SumSec 5400;
	sumsec+totaltime;
run;


* �ش� ������ �ҷ����̱� ;
* ����TotalTime�� ���� ���� ���ϱ� (���� SumSec�� 5400���� �����ؼ� ���ذ��� ������ ����) ;
* ���� TotalTime�� 800���� ū ��쿡 ���� ���ο� ���� TestLength�� 'Long'���� ���� ;
data stress;
	infile tests;
	input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
	RecHR 35-37 TimeMin 39-40 TimeSec 42-43
	Tolerance $ 45;
	TotalTime=(timemin*60)+timesec;
	retain SumSec 5400;
	sumsec+totaltime;
	if totaltime>800 then TestLength='Long';
run;


* �ش� ������ �ҷ����̱� ;
* ����TotalTime�� ���� ���� ���ϱ� (���� SumSec�� 5400���� �����ؼ� ���ذ��� ������ ����) ;
* ���� TotalTime�� ũ�⿡ ���� ���ο� ���� TestLength�� ���� ;
data stress;
	infile tests;
	input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
	RecHR 35-37 TimeMin 39-40 TimeSec 42-43
	Tolerance $ 45;
	TotalTime=(timemin*60)+timesec;
	retain SumSec 5400;
	sumsec+totaltime;
	if totaltime>800 then TestLength='Long';
	else if 750<=totaltime<=800 then TestLength='Normal';
	else if totaltime<750 then TestLength='Short';
run;


* �ش� ������ �ҷ����̱� ;
* ����TotalTime�� ���� ���� ���ϱ� (���� SumSec�� 5400���� �����ؼ� ���ذ��� ������ ����) ;
* ���� TotalTime�� ũ�⿡ ���� ���ο� ���� TestLength�� ���� ;
* ���� RestHr�� 70���� ���� ���� ���� ;
* ���� TestLength�� �ִ� ���� ���� ;
data stress;
	infile tests;
	input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
	RecHR 35-37 TimeMin 39-40 TimeSec 42-43
	Tolerance $ 45;
	if resthr<70 then delete;
	TotalTime=(timemin*60)+timesec;
	retain SumSec 5400;
	sumsec+totaltime;
	length TestLength $ 6;
	if totaltime>800 then testlength='Long';
	else if 750<=totaltime<=800 then testlength='Normal';
	else if totaltime<750 then TestLength='Short';
run;


* �ش� ������ �ҷ����̱� (���� timemin, timesec ����) ;
* ����TotalTime�� ���� ���� ���ϱ� (���� SumSec�� 5400���� �����ؼ� ���ذ��� ������ ����) ;
* ���� Tolerance�� 'D'�� ��츸 ���� ;
* ���� TotalTime�� ũ�⿡ ���� ���ο� ���� TestLength�� ���� ;
* ���� TestLength�� �ִ� ���� ���� ;
data stress(drop=timemin timesec);
	infile tests;
	input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
	RecHR 35-37 TimeMin 39-40 TimeSec 42-43
	Tolerance $ 45;
	if tolerance='D';
	TotalTime=(timemin*60)+timesec;
	retain SumSec 5400;
	sumsec+totaltime;
	length TestLength $ 6;
	if totaltime>800 then testlength='Long';
	else if 750<=totaltime<=800 then testlength='Normal';
	else if totaltime<750 then TestLength='Short';
run;
data stress;
	infile tests;
	input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
	RecHR 35-37 TimeMin 39-40 TimeSec 42-43
	Tolerance $ 45;
	if tolerance='D';
	drop timemin timesec;
	TotalTime=(timemin*60)+timesec;
	retain SumSec 5400;
	sumsec+totaltime;
	length TestLength $ 6;
	if totaltime>800 then testlength='Long';
	else if 750<=totaltime<=800 then testlength='Normal';
	else if totaltime<750 then TestLength='Short';
run;


* �ش� ������ �ҷ����̱� (���� timemin, timesec ����) ;
* ����TotalTime�� ���� ���� ���ϱ� (���� SumSec�� 5400���� �����ؼ� ���ذ��� ������ ����) ;
* ���� Tolerance�� 'D'�� ��츸 ���� ;
* ���� TotalTime�� ũ�⿡ ���� ���ο� ���� TestLength�� Message�� ���� ;
* ���� TestLength�� Message�� �ִ� ���� ���� ;
data stress;
	infile tests;
	input ID $ 1-4 Name $ 6-25 RestHR 27-29 MaxHR 31-33
	RecHR 35-37 TimeMin 39-40 TimeSec 42-43
	Tolerance $ 45;
	TotalTime=(timemin*60)+timesec;
	retain SumSec 5400;
	sumsec+totaltime;
	length TestLength $ 6 Message $ 20;
	if totaltime>800 then
	do;
		testlength='Long';
		message='Run blood panel';
	end;
	else if 750<=totaltime<=800 then testlength='Normal';
	else if totaltime<750 then TestLength='Short';
run;



** �ݺ� (loop) ���� ���� ;


* ���� Earned�� �ݺ������� ���� ���ϱ� ;
data earnings;
	Amount=1000;
	Rate=.075/12;
	do month=1 to 12;
		Earned+(amount+earned)*(rate);
	end;
run;


* ���� Value, Interest, Year�� �ݺ������� ���� ���ϱ� ;
* ���� counter ���� ;
data earn (drop=counter);
	Value=2000;
	do counter=1 to 20;
		Interest=value*.075;
		value+interest;
		Year+1;
	end;
run;


* ���� Value, Interest, Year�� �ݺ������� ���� ���ϱ� ;
* �ݺ������� ��� ����ϱ� ;
* 10���� �ݺ� (���� Year�� ¦���� ����) ;
data earn;
	Value=2000;
	do Year=1 to 20;
		Interest=value*.075;
		value+interest;
	Year+1;
	output;
	end;
run;


* ���� Capital�� �ݺ������� ���� ���ϱ� ;
data earn;
	Capital=2000;
	do month=1 to 12;
		Interest=capital*(.075/12);
		capital+interest;
		output ;
	end;
run;


* ���� loop ���� ;
* ���� Capital�� �ݺ������� ���� ���ϱ� ; 
data earn;
	do year=1 to 20;
		Capital+2000;
		do month=1 to 12;
			Interest=capital*(.075/12);
			capital+interest;
			output ;
		end;
		output ;
	end;
run;


* until(���ǹ��� �����ϴ� ����� ���ö����� �ݺ�) ;
data invest;
	do until(Capital>=50000);
		capital+2000;
		capital+capital*.10;
		Year+1;
		output ;
	end;
run;
data invest;
	do until(Capital>=50000);
		Year+1;
		capital+2000;
		capital+capital*.10;
		output ;
	end;
run;


* while(���ǹ��� �����ϴ� ��쿡 ���ؼ� �ݺ�) ;
data invest;
	do while(Capital<50000);
		capital+2000;
		capital+capital*.10;
		Year+1;
		output ;
	end;
run;


* 2���� ���� �Ͽ��� �����ϴ� �ݺ���� ;
* 10�� �ݺ� �ñ��� Capital�� ���� 50000�� ���� ���� ;
data invest(drop=i);
	do i=1 to 10 until(Capital>=50000);
		Year+1;
		capital+2000;
		capital+capital*.10;
		output ;
	end;
run;


* 2���� ���� �Ͽ��� �����ϴ� �ݺ���� ;
* 10�� �ݺ��ϴ� �߰��� Capital�� ���� 50000�� ���� ;
data invest(drop=i);
	do i=1 to 10 until(Capital>=50000);
		Year+1;
		capital+4000;
		capital+capital*.10;
		output ;
	end;
run;


