import a,os
u = set()
for f in [s for s in dir(a) if not s.startswith("__")]:
    try:
        codeobj = a.__dict__[f].func_code
        filename = os.path.split(codeobj.co_filename)[1]
        print "%s %s:%d" % (f, filename, codeobj.co_firstlineno)
        print "\t"+"\n\t".join(sorted(codeobj.co_names))
    except:
        print f, "***"
#print sorted(u)
