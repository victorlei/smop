#Convert Octave code into python

import numpy as np

def MidnightValues(filename=None,newname=None, dtype = float):
    
#    filename1 = open(filename,"r")    
   # newname1 = open(newname,"w")
    
    Rows2skip = 5;
    Data_Matrix = np.loadtxt((filename,newname))
    Modified_Matrix = [];
    Data_colums = np.size(Data_Matrix);
    print Data_colums(2)
    add_matrix = np.zeros(Rows2skip,Data_colums);
    extended_data = [Data_Matrix,add_matrix];
    new_rows = np.ceil(len(Data_Matrix)/(Rows2skip+1));

    for n in new_rows:
        print(n)
        
        if n < 1:
            Modified_Matrix(n+1).lvalue = float(extended_data(n + 1));
            return float(Modified_Matrix(n+1))
        else:
            Modified_Matrix(n).lvalue = float(extended_data((Rows2skip + 1)*n - Rows2skip));
            return float(Modified_Matrix(n))
        return float("Hydrodata.tek")    
    np.savetxt('Modified_Matrix',newname)          

MidnightValues("Hydrodata.tek","Just.tek")
