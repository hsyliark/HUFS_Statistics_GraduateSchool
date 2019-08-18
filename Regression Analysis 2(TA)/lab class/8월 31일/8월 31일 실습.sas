data class ;                                    /* Data Step */
set sashelp.class ;
run ;

proc print data = class ;
run ;                                               /* Proc Step */

*===================================* ;
*========         Import Data         =======* ;
*===================================* ;
*----------------------------------------------* ;
*-----          Using INFILE Statement         ----------* ;
*----------------------------------------------* ;

* delimiter : ������, firstobs : ù��° ����ġ�� ��ġ ;
* $8. : 8byte���� �б�, best32. : �Ҽ��� �ڸ� ���� ;
data class ;
infile 'D:\�����ڷ�\College, Graduate school\Graduate school (Master)\4th semester\ȸ�ͺм�(TA)\lab class\8�� 31��\Sample_data.csv' 
delimiter=',' firstobs=2 ;
informat Name $8.   Age best32.   Height best32.   Weight best32.    ;
input Name $ Sex $ Age Height Weight ;
run ;

*-----------------------------------------* ;
*--------             Using dataline            ------* ;
*-----------------------------------------* ;

data internal ;
informat name $6. age best12. gender $5. ;
input name $ age gender $ ;
datalines ;
Johan  50 Male
Julia  25 Female
;
run ;

proc print data=internal ;
run ;

*-----------------------------------------* ;
*--------     One to  one Merge             ------* ;
*-----------------------------------------* ;

data personal ;
input name $4. age eyes $6. ;
datalines ;
Joe   23  Blue  
Fred  30  Green
Sue   24  Brown
;
run ;

data business ;
input Name $4. Job $8. Salary ;
datalines ;
Joe  Clerk   20000
Fred Manager 30000
Sue  Cook    24000
;
run ;

/* Sorting the data */

proc sort data=personal ;
by name ;
run ;

proc sort data=business ;
by name ;
run ;

/* Merging both datasets */

data both ;
merge personal business ;
by name ;
run ;

proc print data=both ;
run ;

*---------------------------------------------* ;
*---------                  Mash merge            --------* ;
*---------------------------------------------* ;

data grade1 ;
input id score1 score2 ;
datalines ;
7 20 18
9 15 19
12 9 15
;
run ;

data grade2 ;
input id score3 score4 ;
datalines ;
7 19 12
10 12 20
12 10 19
;
run ;

/* Sorting both datasets */

proc sort data=grade1 ;
by id ;
run ;

proc sort data=grade2 ;
by id ;
run ;

data grade_both ;
merge grade1 grade2 ;
by id ;
run ;

proc print data=grade_both ;
run ;

*========================================* ;
*==========           Functions       ===========* ;
*========================================* ;
*---------------------------------------------------------------* ;
*-------------                   Numeric functions             ----------------* ;
*---------------------------------------------------------------* ;
/* Sum functions */
data sum_ex ;
x1 = sum(2,2,2) ; x2 = sum(4,.,6) ; x3 = sum( of x1-x2) ; x4 = sum( of x1-x3, 100) ; x5 = sum( of x:) ;
run ;

proc print data=sum_ex ;
run ;

*---------------------------------------------------------------* ;
*-------------                Character functions             ----------------* ;
*---------------------------------------------------------------* ;
/* Substring functions */
data substr_exp ;
a = "Chemotherapy+Treatment" ;
b = substr(a,1,12) ; /* 12��° ���ڱ����� ���� */
c = substr(a,1,index(a,"+")-1) ; /* "+"��ȣ �ٷ� ������ ��ġ������ ���� */
run ;

proc print data=substr_exp ;
run ;

** ���⼭���ʹ� �����ǹ��� SAS�� �̿��ؼ� �ǽ� ;

*==========================================* ;
*=========            Statements             =========* ;
*==========================================* ;

/* Drop statement */
data drop_exp ;
set sashelp.class ;
drop height weight ;
run ;

/* Keep statement */
data keep_exp ;
set sashelp.class ;
keep name age ;
run ;

*==========================================* ;
*=========    Data Set options             =========* ;
*==========================================* ;

/* Rename options */
data drop_ex1 ;
set sashelp.class(rename=(age=student_age)) ;
run ;

data drop_ex2(rename=(name=student_name)) ;
set sashelp.class ;
run ;

/* Firstobs */
proc print data=sashelp.class(firstobs=3) ;
run ;

data first_obs_ex2 ;
set sashelp.class(firstobs=5 obs=10) ;
run ;

proc print data=first_obs_ex2 ;
run ;

































