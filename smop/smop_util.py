import numpy as np

#recreates MatLab array assignment behavior
def safe_set(array,indices,value):
    try:
        array[indices]=value
    except:
        for i in range(len(indices)-len(array.shape)):
            array = array[np.newaxis]
        padamount = ()
        indexlist = []
        shapeindex = 0
        for i in range(len(indices)):
            end = 0
            indexlist.append(indices[i])
            if indices[i] == 'shape':
                if np.isscalar(value):
                    end = array.shape[i]-1
                    indexlist[i] = slice(array.shape[i])
                else:
                    end = value.shape[shapeindex]-1
                    indexlist[i] = slice(value.shape[shapeindex])
                    shapeindex += 1
            elif type(indices[i]) == slice:
                end = indices[i].stop
            else:
                end = indices[i]
            padamount += ((0,max(0,end-array.shape[i]+1)),)
        array = np.pad(array,padamount,'constant')
        array[indexlist]=value
    return array
