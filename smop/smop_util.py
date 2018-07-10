import numpy as np

#recreates MatLab array assignment behavior
def safe_set(array,indices,value):
    try:
        array[indices]=value
    except:
        for i in range (len(indices)-len(array.shape)):
            array = array[np.newaxis]
        padamount = ()
        for i in range(len(indices)):
            padamount += ((0,max(0,indices[i]-array.shape[i]+1)),)
        array = np.pad(array,padamount,'constant')
        array[indices]=value
    return array
