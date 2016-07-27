function res = mybench(varargin) 
% res = mybench('noOfRepeats', 3,...
%                     'normalize',true,...
%                     'onlyTests',[],...
%                     'imagep',true)
%
% Benchmark script for MATLAB and Octave.
% Tested on Matlab 2009a and Octave 3.2.2 for Windows.
%
% Execution time of basic matrix manipulation function is tested along with 
% integration, solving nonlinear equation, image processing functions ( if
% avaliable), saveing/loading matrices to a file system, for loop,
% binary operation,etc. In total 27
% tests are performed (less if image processing functions not avaliable).
%
% All results are normilized against the results obtained
% using MATLAB 7.4.0.287 (R2007a) on Intel Mac OS X 10.4.11 (Intel Core Duo
% 2GHz, 2GB RAM)
%
% At the end, arithmetic and geometric means of the times obtained are
% calculated. All results obtained are stored in a txt file named
% results_<matlab/octave version>.txt.
%
% INPUT
%  noOfRepeats - int - number of times each test is executed (default 3)
%  normalize - boolean - normalize results (default true).
%  onlyTests - int vector - do only tests given (default [], i.e. do all tests) 
%
% OUTPUT
% res - struct -   normalized geometric mean . 
%
% EXAMPLES
% res = mybench(); %perform all tests with default settings.
% res = mybench('noOfRepeats',10); %perform all tests 10 times.
% res = mybench('onlyTests',[1,5,8]); %perform only tests 1,5 and 8.
% res = mybench('noOfRepeats', 1,'normalize',false); % repeat 1 time 
%                                               %each tests and do not 
%                                               %normalize results.
%
% KNOWN_ISSUES
% Solving nonlinear equation produces info about termination - don't worry.
% 
% Site: http:\\shortrecipes.blogspot.com
% Date: Nov 2009
%
    global IS_OCTAVE IS_IMAGE_PROCESSING

    %DEFAULT INPUT PARAMETERS.
    conf = struct(...
        'noOfRepeats', 3,...
        'normalize',true,...
        'imagep',true,...
        'onlyTests',[]...
        );
    
    conf = getargs(conf, varargin); 
  
    IS_OCTAVE = exist('OCTAVE_VERSION','builtin') > 0;
    IS_IMAGE_PROCESSING = false;
    NO_REPETITIONS = conf.noOfRepeats;
    NORMALIZE_TIMES = conf.normalize;
    
    if exist('imrotate','file') > 0 && exist('imresize','file') > 0  ...
            && exist('imerode','file') > 0 && conf.imagep == true
         disp('Image processing toolbox found');
         IS_IMAGE_PROCESSING = true;
    end
    
    if conf.noOfRepeats < 1
        conf.noOfRepeats = 1;
    end
    
    clc;

    mytests=getBenchmarkTests();
    noOftests = length(mytests);    

    %create output file
    moVersio =     strrep(version(),' ','_');
    outFileName = "foo.txt" %['results_',moVersio,'.txt'];
    fid = fopen(outFileName,'w');
    
    if NORMALIZE_TIMES
        fprintf(fid,'%s\t%s\t%s\n',['Name_',moVersio],'Time','Norm_time');
    else
        fprintf(fid,'%s\t%s\n',['Name_',moVersio],'Time_[s]');
    end
    
    avarage_time = 0;
    
    times_vector =[];
    times_vector1 =[];  % not normalized
    
    if isempty(conf.onlyTests) 
        doTheseTests = 1:noOftests;
    else
        doTheseTests = conf.onlyTests;
        noOftests = length(conf.onlyTests);
    end
    
    %loop over tests
    for i=doTheseTests
       
       %display some info
       fprintf(1,'Execute test %d/%d -  %s\n',i,noOftests,...
           mytests{i}.name);       
       if IS_OCTAVE,  fflush(stdout);   end
       
       try
           %get input for a give test 
            x = mytests{i}.input();
            
           %execute test and measure time
           cumulative_time = 0;
           cumulative_time1 = 0;
           goldResult = 1;
           for ii=1:NO_REPETITIONS
                
                fprintf(1,'%d ',ii);
                if IS_OCTAVE, fflush(stdout); end
                
                t0=tic();   
                mytests{i}.test(x); 
                t1=toc(t0); 
                
                if isfield(mytests{i}, 'goldResult') && NORMALIZE_TIMES == true       
                    goldResult = mytests{i}.goldResult;
                 end
                
                cumulative_time=cumulative_time+t1/goldResult;
                cumulative_time1=cumulative_time1+t1;
                
           end
           avarage_time = cumulative_time/NO_REPETITIONS;
           avarage_time1 = cumulative_time1/NO_REPETITIONS;
           times_vector(end+1) = avarage_time;
           times_vector1(end+1) = avarage_time1; % not normalized
       
       catch
           le = lasterror;
           disp(le.message);
           fprintf(1,'\n\n \t  ... Skip to the next test ...\n\n');
           if IS_OCTAVE, fflush(stdout); end
           continue
       end
       
       
       %some postprocessing if defined
       if isfield(mytests{i}, 'post')         
           mytests{i}.post();
       end
       
       
       %display some info       
       
       fprintf(1,'\n\tTime %.2f [s]\n',avarage_time1);    
       if NORMALIZE_TIMES == true
           fprintf(1,'\tNormalized time %.2f \n',avarage_time);
       end
       fprintf(1,'\n');
       if IS_OCTAVE, fflush(stdout);  end
       
       if NORMALIZE_TIMES
            fprintf(fid,'%s\t%f\t%f\n',mytests{i}.name,...
                avarage_time1,avarage_time);      
       else
           fprintf(fid,'%s\t%f\n',mytests{i}.name,...
                avarage_time1);  
       end
    end

    times_product  = prod(times_vector);
    times_mean  = mean(times_vector);
    times_geometric_mean = times_product^(1/length(times_vector) );
    
    times_product1  = prod(times_vector1); % not normalized
    times_mean1  = mean(times_vector1);
    times_geometric_mean1 = times_product1^(1/length(times_vector1) );
    
    res.norm_geometric_mean = times_geometric_mean;
    res.norm_arithmetic_mean = times_mean;
    res.norm_min = min(times_vector);
    res.norm_max = max(times_vector);
    
    fprintf(1,'\n\t --- SUMMARY ---\n');
    
     %display some info       
       
     fprintf(1,'\n\tMean: geometric %.3f [s], arithmetic %.3f [s]',...
         times_geometric_mean1,times_mean1);
     fprintf(1,'\n\tMin %.3f [s], Max %.3f [s]\n\n',...
         min(times_vector1),max(times_vector1)); 
 
          
     if NORMALIZE_TIMES == true
          fprintf(1,'\tNormalized Mean: geometric %.3f, arithmetic %.3f',...
            times_geometric_mean,times_mean);
          fprintf(1,'\n\tNormalized Min %.3f [s], Max %.3f [s]\n\n',...
            min(times_vector),max(times_vector));  
     end
     if IS_OCTAVE, fflush(stdout);  end
    
     if NORMALIZE_TIMES
      fprintf(fid,'%s\t%f\t%f\n','Geom_mean',times_geometric_mean1,...
          times_geometric_mean);     
     else
         fprintf(fid,'%s\t%f\t%f\n','Geom_mean',times_geometric_mean1);     
     end
    
    fclose(fid);
    
    disp('');
    disp(['End of test. File ',outFileName,' was created.'])
    
    %do some clean up
    if exist('out_gray.png'), delete 'out_gray.png'; end
    if exist('out_1.png'), delete 'out_1.png'; end
    if exist('out_mtx'), delete 'out_mtx'; end
    if exist('out_mtx.mat'), delete 'out_mtx.mat'; end
    if exist('dlm.txt'), delete 'dlm.txt'; end
    
    clear IS_OCTAVE IS_IMAGE_PROCESSING;

%%%%%%%%%%%%%%%%%%%%%%%%%% FUNCTIONS **********************
    
function s = getBenchmarkTests()
%the cell with tests name, test functions and input params.
%Each tests has the form of a structure with filelds 'name', 'test',
%'input', and optional 'post' nad 'goldResult' fields.
%'name' is a name that you want to give to  your test.
%'test' is an anonymous function or a function handler. The execution time
% of this function is measured.
%'input'  anonymous function that provides input data to a test. The time
% of this function is not measured.
% 'post'  anonymous function that can do some postprocessing, e.g. cleanup.
% the time of 'post' funciton is not measured
% 'goldResult' is a result in seconds obtaiend on my computer. This times
% is used for the normalization of time scores. 
%
%
    global IS_OCTAVE IS_IMAGE_PROCESSING         
    
    s={};     
  
    %s{end+1}=struct('name','interp2','test', @(x) interp2(x,2,'spline'),...
    %    'input', @()rand(600),'goldResult',4.20);
    

    s{end+1}=struct(...
        'name','rand',...
        'test', @(x) rand(x),...
        'input', @()4000,...
        'goldResult',1.12); 
    
    s{end+1}=struct(...
        'name','randn',...
        'test', @(x) randn(x),...
        'input', @()4000,...
        'goldResult',0.58);  
    
    s{end+1}=struct('name','primes','test', @(x) primes(x), 'input', @() 1e7,...
        'goldResult',0.74);
    
    s{end+1}=struct('name','fft2','test', @(x) fft2(x), 'input', @()rand(3000),...
        'goldResult',2.28);
   
   % s{end+1}=struct('name','ifft2','test', @(x) ifft2(x), 'input', @()rand(3000),...
   %    'goldResult',2.979296);

    s{end+1}=struct('name','square','test', @(x) x^2, 'input', @()rand(1000),...
        'goldResult',1.35);
     
    s{end+1}=struct('name','inv','test', @(x) inv(x), 'input', @()rand(1000),...
        'goldResult',0.87);
    
    s{end+1}=struct('name','eig','test', @(x) eig(x), 'input', @()rand(1000),...
        'goldResult',9.45);
    
    s{end+1}=struct('name','qr','test', @(x) qr(x), 'input', @()rand(1000),...
        'goldResult',0.79);
    
    s{end+1}=struct('name','schur','test', @(x) schur(x), 'input', @()rand(600),...
        'goldResult',2.67);
    
    s{end+1}=struct('name','roots','test', @(x) roots(x), 'input', ...
        @()rand(600,1),'goldResult',2.08);
    
     s{end+1}=struct('name','binary',...
         'test', @(x) eye(x)<1,...
         'input', @() 5000 ,...
         'goldResult',0.51);
     
     s{end+1}=struct('name','forLoop',...
         'test', @(x)forLoop(x),...
         'input', @() 200 ,...
         'goldResult',0.06);
    
      s{end+1}=struct('name','makeSparse',...
         'test', @(x) sparse(x),...
         'input', @() eye(5000) ,...
         'goldResult',0.49);
    
      s{end+1}=struct('name','multiplySparse',...
         'test', @(x) sparse(x)*sparse(x),...
         'input', @() eye(5000)*rand(1) ,...
         'goldResult',0.98);
     
     s{end+1}=struct('name','sLinearEq',...
         'test', @(x) magic(x)/rand(1,x),...
         'input', @() 2000 ,...
         'goldResult',1.94);
    
  %  s{end+1}=struct('name','sNonLinearEq','test',...
   %     @(x) solveNonLinearEq(),'input', @() NaN ,'goldResult',0.07);
    
    s{end+1}=struct('name','saveLoadMtx','test',...
        @(x) saveLoadMtx(x),'input', @() rand(1000),'goldResult',0.93);
    
    s{end+1}=struct('name','dlmwriteRead','test',...
        @(x) dlmwriteRead(x),'input', @() rand(500),'goldResult',5.03);
        
   s{end+1}=struct('name','median','test', @(x) median(x(:)),...
       'input', @() rand(4000), 'goldResult',3.32);
   
   s{end+1}=struct('name','std','test', @(x) std(x(:)),...
       'input', @() rand(4000),'goldResult',0.84);
   
 %  s{end+1}=struct('name','quadl','test',...
  %     @() quadl (@(x) x .* sin (1 ./ x) .* sqrt (abs (1 - x)), 0, 3),...
   %    'input', @() NaN,'goldResult',0.038028);
  
  
   if IS_IMAGE_PROCESSING
             
        s{end+1}=struct('name','doImgAndSaveAsPNG','test',...
           @(x) doImgAndSaveAsPNG(x),'input',...
            @() rand(1500,1500,3) ,'post', @() pause(2),...
          'goldResult',2.00 ); 
         
        s{end+1}=struct('name','imageToGray','test', @(I) imageToGray(I),...
            'input', ...
            @()  imread('out_1.png'),'goldResult',0.56  );
        
        s{end+1}=struct('name','imageRotate','test', @(I)  imageRotate(I),...
            'input',...
           @() imread('out_gray.png'), 'goldResult',2.94 );
       
        s{end+1}=struct('name','imresize','test', @(I) imresize(I,1.2),...
            'input',...
            @() imread('out_gray.png'), 'goldResult',1.24);
        
        s{end+1}=struct('name','imageFilter','test', @(I) imageFilter(I),...
            'input',...
            @() imread('out_gray.png'), 'goldResult',0.20); 
        
        s{end+1}=struct('name','imageErode','test', @(I) imageErode(I),...
            'input',...
            @() imread('out_gray.png'), 'goldResult',0.46 );  
        
        s{end+1}=struct('name','medfilt2','test', @(I) medfilt2(I),...
            'input',...
            @() magic(2000), 'goldResult',1.03 ); 
        
   end
   
 % ADDITIONAL TEST FUNCTIONS
function saveLoadMtx(out_mtx) 
    save out_mtx;
    clear out_mtx;
    load out_mtx;        

function dlmwriteRead(x) 
    dlmwrite('dlm.txt', x);  
    dlmread('dlm.txt');
    
    
function forLoop(x) 
    for i=1:x
        for j=1:x
             for k=1:x
                i+j+k;
             end
        end        
    end
    
function doImgAndSaveAsPNG(x)
    %plot a surf and save it image png with 300DPI
	%f=figure;
	%set(f,'Visible','off');
	%surf(x);    
	%print(['-f',int2str(f)],'-dpng','-r200',['out_1.png']);
	%close(f)
    
    imwrite(x,'out_1.png');


function solveNonLinearEq() 
    [x, fval, info] = fsolve (@equationsToSolve, [1; 1;1;1]);

function y = equationsToSolve (x)
  y(1) = -2*x(1)^2 + 3*x(1)*x(2)  + 4*sin(x(2)) + log(x(3)) - 6;
  y(2) = 3*x(1)^2 - 2*x(2)*x(2)^2 + 3*cos(x(1)) + 4;
  y(3) = 1*x(1)^2 - 2*x(1)*x(2)*x(4)^2 + 3*cos(x(1)) + 4;
  y(4) = 1*x(1)^2 - 2*x(1)*x(2)*x(3)^2 + 3*cos(x(4)) + 4;
  
  
function imageToGray(I)
    Igray = rgb2gray(I);
    imwrite(Igray,'out_gray.png');

function imageRotate(I)
    I2=imrotate(I,2);
    
function  imageFilter(I) 
    h=fspecial('sobel');
    filteredI = imfilter(I, h);
    
function  imageErode(I) 
    SE=eye(5);
    erodedI = imerode(I, SE);  
    
    
    
    
% Get input argumetns
function defs = getargs(defs, varglist)
    l=length(varglist);
    if l==0, return, end
    if mod(l,2) ~=0, 
        disp(' !!! Odd number of parameters !!!'); 
        defs={};
        return
    end
    varnames={varglist{1:2:l}};
    varvalues={varglist{2:2:l}};
    given_vars=zeros(1,l/2);
    for i=1:1:l/2
        existss=isfield(defs,varnames{i});
        given_vars(i)=existss;
    end
 
    if min(given_vars)==0, 
        disp('!!! No such parameter(s):');
        disp(varnames(~given_vars));
        defs={};
        return
    end
    for i=1:1:l/2
       defs.(varnames{i}) = varvalues{i};
    end     
