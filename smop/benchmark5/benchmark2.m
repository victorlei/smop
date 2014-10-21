%***************************************************************************
%* Matlab Benchmark program version 5.0 *
%* Author : Stefan Steinhaus *
%*
EMAIL : stefan@steinhaus
-
net.de *
%* This program is public domain. Feel free to copy it freely. *
%***************************************************************************
clc
disp('The following b
enchmark program will print the average timings')
disp('to calculate the functions by 3 runs.')
disp(' ')
disp(' ')
disp('!!! MATLAB
-
Benchmarkprogram !!!')
disp('=================================')
disp(' ')
%* Misc. operation *
result=0; runs=3;
for i
=1:runs;
tic;
datmat=importdata('currency2.txt',' ');
Limits_Area=[1,261,522,784,1045,1305,1565,1827,2088,2349,2610,2871,3131,3158];
for k=1:2000;
for j=1:13;
Year_Data=datmat.data(Limits_Area(j):Limits
_Area(j+1)
-
1,4:37);
Min_Year=min(Year_Data);
Max_Year=max(Year_Data);
Mean_Year=mean(Year_Data);
GainLoss_Year=100
-
(Year_Data(1,:)+0.00000001)./(Year_Data(end,:)+0.00000001)*100;
end;
end;
result=result+toc;
e
nd;
result=result/runs;
disp(['IO test & descriptive statistics_____________________ : ' num2str(result) ' sec.'])
result=0; a=1;
for i=1:runs
tic;
for x=1:15000;
for y=1:15000;
a=a+x+y;
end;
end;
result=result+toc;
en
d;
result=result/runs;
disp(['Loop testing_________________________________________ : ' num2str(result) ' sec.'])
result=0;
for i=1:runs
for j=1:200
b=1+(rand(2000,2000)/100);
tic;
a=b.^1000;
result=result+toc;
end
end
re
sult=result/runs;
disp(['2000x2000 normal distributed random matrix^1000______ : ' num2str(result) ' sec.'])
clear a; clear b; result=0;
for i=1:runs
for j=1:100
a=rand(1000000,1);
tic;
b=sort(a);
result=result+toc;
end
end
result=result/runs;
disp(['1000000 values sorted ascending______________________ : ' num2str(result) ' sec.'])
%* Analysis *
clear a; clear b; result=0;
for i=1:runs
for j=1:100
a=rand(1048576,1);
tic;
b=fft(a);
result=
result+toc;
end
end
result=result/runs;
disp(['FFT over 1048576 values (2^20)_______________________ : ' num2str(result) ' sec.'])
%* Algebra *
clear a; clear b; result=0;
for i=1:runs
for j=1:100
a=rand(1500,1500);
tic;
b=d
et(a);
-
10
-
result=result+toc;
end
end
result=result/runs;
disp(['Determinant of a 1500x1500 random matrix_____________ : ' num2str(result) ' sec.'])
clear a; clear b; result=0;
for i=1:runs
for j=1:100
a=rand(1500,1500);
tic;
b=inv(a);
result=result+toc;
end
end
result=result/runs;
disp(['Inverse of a 1500x1500 uniform distr. random matrix__ : ' num2str(result) ' sec.'])
clear a; clear b; result=0;
for i=1:runs
a=rand(1200,1200);
tic;
c=eig(a);
re
sult=result+toc;
end
result=result/runs;
disp(['Eigenval. of a normal distr. 1200x1200 randommatrix___ : ' num2str(result) ' sec.'])
clear a; clear b; result=0;
for i=1:runs
for j=1:100
a=rand(1500,1500);
a=a'*a;
tic;
b=chol(
a);
result=result+toc;
end
end
result=result/runs;
disp(['Cholesky decomposition of a 1500x1500
-
matrix_________ : ' num2str(result) ' sec.'])
clear a; clear b; result=0;
for i=1:runs
for j=1:100
a=rand(1500,1500);
tic;
b=
a'*a;
result=result+toc;
end
end
result=result/runs;
disp(['1500x1500 cross
-
product matrix_______________________ : ' num2str(result) ' sec.'])
%* Number theory *
clear a; clear b;
phi = 1.6180339887498949; result=0;
for i=1:runs;
a=floor(1
000*rand(10000000,1));
tic;
b=(phi.^a
-
(
-
phi).^(
-
a))/sqrt(5);
result=result+toc;
end
result=result/runs;
disp(['Calculation of 10000000 fibonacci numbers____________ : ' num2str(result) ' sec.'])
%* Stochastic
-
statistic *
result=0; a=0; b=0;
for
i=1:runs;
a=rand(10000,1000);
tic;
b=princomp(a);
result=result+toc;
end;
result=result/runs;
disp(['Calc. of the principal components of a 10000x1000 matrix : ' num2str(result) ' sec.'])
clear a; clear b; result=0;
for i=1:runs
for j=1:100
a=rand(1500,1500);
tic;
b=gamma(a);
result=result+toc;
end
end
result=result/runs;
disp(['Gamma function over a 1500x1500 matrix_______________ : ' num2str(result) ' sec.'])
clear a; clear b; result=0;
for i=1:runs
for j
=1:100
a=rand(1500,1500);
tic;
b=erf(a);
result=result+toc;
end
end
result=result/runs;
disp(['Gaussian error function over a 1500x1500 matrix______ : ' num2str(result) ' sec.'])
clear a; clear b; result=0;
for i=1:runs
f
or j=1:100
-
11
-
a=randn(1000,1000);
b=1:1000;
tic;
b=a
\
b';
result=result+toc;
end
end
result=result/runs;
disp(['Linear regression over a 1000x1000 matrix____________ : ' num2r(result) ' sec.']s
