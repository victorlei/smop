/*

Copyright (C) 2007, 2013 Michael Goffioul

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

package org.octave;

import java.lang.reflect.*;

public class ClassHelper
{
  private static OctClassLoader loader;

  static
  {
    ClassLoader l = ClassHelper.class.getClassLoader ();
    loader = (l instanceof OctClassLoader ? (OctClassLoader) l :
              new OctClassLoader (l));
  }


  /**
   * Add the given path to the classpath.
   * @param name String - path to addd to the classpath
   * @return boolean - true if the given path exists and was added to the classpath.
   * @throws Exception
   */
  public static boolean addClassPath (String name)
    throws Exception
  {
    boolean found = false;
    java.io.File f = new java.io.File (name);
    if (f.exists ())
      {
        loader.addClassPath (name);
        found = true;
      }
    return (found);
  }


  // new -MH-
  /**
   *
   * @param name String - path to remove from classpath.
   * @return boolean - true if the given path existed in the classpath before it was removed.
   * @throws Exception
   */
  public static boolean removeClassPath (String name)
    throws Exception
  {
    boolean found = false;
    java.io.File f = new java.io.File (name);
    java.net.URL urlToRemove = f.toURI ().toURL ();
    // save urls in current class path
    java.net.URL[] urls = loader.getURLs ();

    // create a completely new class loader because java.net.URLClassLoader has no removeClassPath() method
    ClassLoader l = ClassHelper.class.getClassLoader ();
    loader = (l instanceof OctClassLoader ? (OctClassLoader) l :
              new OctClassLoader (l));

    // add the previous urls back, except for the one
    for (int i = 0; i < urls.length; i++)
      {
        java.net.URL url = urls[i];
        if (!url.equals (urlToRemove))
          {
            loader.addURL (url);
          }
        else
          {
            // path to remove
            found = true;
          }
      }

    return (found);
  }


  public static String getClassPath ()
  {
    StringBuffer buf = new StringBuffer ();
    String pathSep = System.getProperty ("path.separator");
    java.net.URL[] urls = loader.getURLs ();

    for (int i = 0; i < urls.length; i++)
      {
        try
          {
            java.io.File f = new java.io.File (urls[i].toURI ());
            if (buf.length () > 0)
              {
                buf.append (pathSep);
              }
            buf.append (f.toString ());
          }
        catch (java.net.URISyntaxException ex)
          {}
      }
    return buf.toString ();
  }


  // new -MH-
  // return list of methods for given class name
  public static String getMethods (String classname)
    throws ClassNotFoundException
  {
    return (getMethods (Class.forName (classname)));
  }


  // new -MH-
  // return list of methods for given class instance
  public static String getMethods (Object obj)
    throws ClassNotFoundException
  {
    return (getMethods (obj.getClass ()));
  }


  // new -MH-
  // return list of methods for given class
  public static String getMethods (Class klass)
  {
    StringBuffer sb = new StringBuffer ();

    Method theMethod[] = klass.getMethods ();
    for (int i = 0; i < theMethod.length; i++)
      {
        if (i > 0)
          {
            sb.append (";");
          }
        sb.append (theMethod[i].getReturnType ().getCanonicalName ());
        sb.append (" ");
        sb.append (theMethod[i].getName ());
        sb.append ("(");

        Class theParameter[] = theMethod[i].getParameterTypes ();
        for (int j = 0; j < theParameter.length; j++)
          {
            if (j > 0)
              {
                sb.append (", ");
              }
            sb.append (theParameter[j].getCanonicalName ());
          }
        sb.append (")");

        Class theExceptions[] = theMethod[i].getExceptionTypes ();
        if (theExceptions.length > 0)
          {
            sb.append (" throws ");
            for (int j = 0; j < theExceptions.length; j++)
              {
                if (j > 0)
                  {
                    sb.append (", ");
                  }
                sb.append (theExceptions[j].getCanonicalName ());
              }
          }
      }

    return (sb.toString ());
  }


  // new -MH-
  // return list of fields for given class
  public static String getFields (Class klass)
  {
    StringBuffer sb = new StringBuffer ();

    Field theField[] = klass.getFields ();
    for (int i = 0; i < theField.length; i++)
      {
        if (i > 0)
          {
            sb.append (";");
          }
        sb.append (theField[i].getName ());
      }

    return (sb.toString ());
  }


  // new -MH-
  // return list of fields for given class name
  public static String getFields (String classname)
    throws ClassNotFoundException
  {
    return (getFields (Class.forName (classname)));
  }


  // new -MH-
  // return list of fields for given class instance
  public static String getFields (Object obj)
    throws ClassNotFoundException
  {
    return (getFields (obj.getClass ()));
  }


  public static Method findMethod (Class cls, String name, Class[] argTypes)
  {
    try
      {
        return cls.getMethod (name, argTypes);
      }
    catch (Exception e)
      {
        Method[] mList = cls.getMethods ();
        Method m;
        for (int i = 0; i < mList.length; i++)
          {
            m = mList[i];
            if (m.getName ().equals (name)
                && m.getParameterTypes ().length == argTypes.length
                && isCallableFrom (m, argTypes))
              {
                return m;
              }
          }
        return null;
      }
  }


  public static Constructor findConstructor (Class cls, Class[] argTypes)
  {
    try
      {
        return cls.getConstructor (argTypes);
      }
    catch (Exception e)
      {
        Constructor[] cList = cls.getConstructors ();
        //System.out.println("# constructors: " + cList.length);
        Constructor c;
        for (int i = 0; i < cList.length; i++)
          {
            //System.out.println("Considering constructor: " + cList[i]);
            c = cList[i];
            if (c.getParameterTypes ().length == argTypes.length
                && isCallableFrom (c, argTypes))
              {
                return c;
              }
          }
        return null;
      }
  }


  private static Object invokeMethod (Method m, Object target, Object[] args)
    throws Exception
  {
    try
      {
        return m.invoke (target, args);
      }
    catch (IllegalAccessException ex)
      {
        String mName = m.getName ();
        Class[] pTypes = m.getParameterTypes ();
        Class currClass = target.getClass ();

        while (currClass != null)
          {
            try
              {
                Method meth = currClass.getMethod (mName, pTypes);
                if (!meth.equals (m))
                  {
                    return meth.invoke (target, args);
                  }
              }
            catch (NoSuchMethodException ex2)
              {}
            catch (IllegalAccessException ex2)
              {}

            Class[] ifaceList = currClass.getInterfaces ();
            for (int i = 0; i < ifaceList.length; i++)
              {
                try
                  {
                    Method meth = ifaceList[i].getMethod (mName, pTypes);
                    return meth.invoke (target, args);
                  }
                catch (NoSuchMethodException ex2)
                  {}
                catch (IllegalAccessException ex2)
                  {}
              }

            currClass = currClass.getSuperclass ();
          }

        throw ex;
      }
  }


  public static Object invokeMethod (Object target, String name,
                                     Object[] args, Class[] argTypes)
    throws Throwable
  {
    Method m = findMethod (target.getClass (), name, argTypes);
    if (m != null)
      {
        try
          {
            Object result = invokeMethod (m, target,
                                          castArguments (args, argTypes,
                                                         m.getParameterTypes ()));
            return result;
          }
        catch (InvocationTargetException ex)
          {
            throw ex.getCause ();
          }
      }
    else
      {
        throw new NoSuchMethodException (name);
      }
  }


  public static Object invokeStaticMethod (String cls, String name,
                                           Object[] args, Class[] argTypes)
    throws Throwable
  {
    Method m = findMethod (Class.forName (cls, true, loader), name,
                           argTypes);
    if (m != null)
      {
        try
          {
            Object result = m.invoke (null,
                                      castArguments (args, argTypes,
                                                     m.getParameterTypes ()));
            return result;
          }
        catch (InvocationTargetException ex)
          {
            throw ex.getCause ();
          }
      }
    else
      {
        throw new NoSuchMethodException (name);
      }
  }


  public static Object invokeConstructor (String cls, Object[] args,
                                          Class[] argTypes)
    throws Throwable
  {
    Constructor c = findConstructor (Class.forName (cls, true, loader),
                                     argTypes);
    if (c != null)
      {
        try
          {
            Object result = c.newInstance (castArguments (args, argTypes,
                                                          c.getParameterTypes ()));
            return result;
          }
        catch (InvocationTargetException ex)
          {
            throw ex.getCause ();
          }
      }
    else
      {
        throw new NoSuchMethodException (cls);
      }
  }


  public static Object getField (Object target, String name)
    throws Throwable
  {
    try
      {
        Field f = target.getClass ().getField (name);
        return f.get (target);
      }
    catch (NoSuchFieldException ex)
      {
        try
          {
            return invokeMethod (target, name, new Object[0], new Class[0]);
          }
        catch (NoSuchMethodException ex2)
          {
            throw ex;
          }
      }
  }


  public static Object getStaticField (String cls, String name)
    throws Throwable
  {
    try
      {
        Field f = Class.forName (cls, true, loader).getField (name);
        return f.get (null);
      }
    catch (NoSuchFieldException ex)
      {
        try
          {
            return invokeStaticMethod (cls, name, new Object[0], new Class[0]);
          }
        catch (NoSuchMethodException ex2)
          {
            throw ex;
          }
      }
  }


  public static void setField (Object target, String name, Object value)
    throws Exception
  {
    Field f = target.getClass ().getField (name);
    f.set (target, castArgument (value, value.getClass (), f.getType ()));
  }


  public static void setStaticField (String cls, String name, Object value)
    throws Exception
  {
    Field f = Class.forName (cls, true, loader).getField (name);
    f.set (null, castArgument (value, value.getClass (), f.getType ()));
  }


  private static boolean isCallableFrom (Method m, Class[] argTypes)
  {
    Class[] expTypes = m.getParameterTypes ();
    for (int i = 0; i < argTypes.length; i++)
      {
        if (!isCallableFrom (expTypes[i], argTypes[i]))
          {
            return false;
          }
      }
    return true;
  }


  private static boolean isCallableFrom (Constructor c, Class[] argTypes)
  {
    Class[] expTypes = c.getParameterTypes ();
    for (int i = 0; i < argTypes.length; i++)
      {
        if (!isCallableFrom (expTypes[i], argTypes[i]))
          {
            return false;
          }
      }
    return true;
  }


  private static boolean isCallableFrom (Class expCls, Class argCls)
  {
    //System.out.println("isCallableFrom: "+expCls.getCanonicalName() + " <=? " + argCls.getCanonicalName());
    if (argCls == null)
      {
        return!expCls.isPrimitive ();
      }
    else if (expCls.isAssignableFrom (argCls))
      {
        return true;
      }
    else if ((isNumberClass (expCls) || isBooleanClass (expCls))
             && isNumberClass (argCls))
      {
        return true;
      }
    else if (isCharClass (expCls) && argCls.equals (Character.class))
      {
        /*
          modified into a more strict check to avoid char to string matching
          to avoid matching method signatureslike
          java_method(char) with octave_call('a String')
          Date: 28-08-2010
          Author: Martin Hepperle
        */
        return true;
      }
    else if (isStringClass (expCls) && argCls.equals (String.class))
      {
        /*
          added for strict String to String matching
          java_method(String) with octave_call('a String')
          but not
          java_method(char) with octave_call('a String')
          Date: 28-08-2010
          Author: Martin Hepperle
        */
        return true;
      }
    else if (expCls.isArray () && argCls.isArray ()
             && isCallableFrom (expCls.getComponentType (),
                                argCls.getComponentType ()))
      {
        return true;
      }
    else if (expCls.equals (Object.class) && argCls.isPrimitive ())
      {
        return true;
      }
    else
      {
        return false;
      }
  }


  private static boolean isNumberClass (Class cls)
  {
    return (cls.equals (Integer.TYPE) || cls.equals (Integer.class)
            || cls.equals (Short.TYPE) || cls.equals (Short.class)
            || cls.equals (Long.TYPE) || cls.equals (Long.class)
            || cls.equals (Float.TYPE) || cls.equals (Float.class)
            || cls.equals (Double.TYPE) || cls.equals (Double.class));
  }


  private static boolean isBooleanClass (Class cls)
  {
    return (cls.equals (Boolean.class) || cls.equals (Boolean.TYPE));
  }


  private static boolean isCharClass (Class cls)
  {
    return (cls.equals (Character.class) || cls.equals (Character.TYPE));
  }


  /**
   * Check whether the supplied class is a String class.
   *
   * Added for more strict char/string mathicng of method signatures
   * Date: 28-08-2010
   * Author: Martin Hepperle
   * @param cls Class - the class to check
   * @return boolean - true if clas is of class java.lang.String
   */
  private static boolean isStringClass (Class cls)
  {
    return (
            cls.equals (String.class)
            );
  }


  private static Object[] castArguments (Object[] args, Class[] argTypes,
                                         Class[] expTypes)
  {
    for (int i = 0; i < args.length; i++)
      {
        args[i] = castArgument (args[i], argTypes[i], expTypes[i]);
      }
    return args;
  }


  private static Object castArgument (Object obj, Class type, Class expType)
  {
    // System.out.println("expType:"+expType.getCanonicalName() + " <= type:" + type.getCanonicalName());
    if (type == null || expType.isAssignableFrom (type))
      {
        return obj;
      }
    else if (isNumberClass (expType))
      {
        if (expType.equals (Integer.TYPE) || expType.equals (Integer.class))
          {
            return new Integer (((Number) obj).intValue ());
          }
        else if (expType.equals (Double.TYPE) || expType.equals (Double.class))
          {
            return new Double (((Number) obj).doubleValue ());
          }
        else if (expType.equals (Short.TYPE) || expType.equals (Short.class))
          {
            return new Short (((Number) obj).shortValue ());
          }
        else if (expType.equals (Long.TYPE) || expType.equals (Long.class))
          {
            return new Long (((Number) obj).longValue ());
          }
        else if (expType.equals (Float.TYPE) || expType.equals (Float.class))
          {
            return new Float (((Number) obj).floatValue ());
          }
      }
    else if (isBooleanClass (expType))
      {
        return new Boolean (((Number) obj).intValue () != 0);
      }
    else if (isCharClass (expType))
      {
        String s = obj.toString ();
        if (s.length () != 1)
          {
            throw new ClassCastException ("cannot cast " + s + " to character");
          }
        return new Character (s.charAt (0));
      }
    else if (expType.isArray () && type.isArray ())
      {
        return castArray (obj, type.getComponentType (),
                          expType.getComponentType ());
      }
    else if (type.isPrimitive ())
      {
        return obj;
      }
    return null;
  }


  private static Object castArray (Object obj, Class elemType,
                                   Class elemExpType)
  {
    int len = Array.getLength (obj);
    Object result = Array.newInstance (elemExpType, len);
    for (int i = 0; i < len; i++)
      {
        Array.set (result, i,
                   castArgument (Array.get (obj, i), elemType, elemExpType));
      }
    return result;
  }


  private static int getArrayClassNDims (Class cls)
  {
    if (cls != null && cls.isArray ())
      {
        return (1 + getArrayClassNDims (cls.getComponentType ()));
      }
    else
      {
        return 0;
      }
  }


  private static Class getArrayElemClass (Class cls)
  {
    if (cls.isArray ())
      {
        return getArrayElemClass (cls.getComponentType ());
      }
    else
      {
        return cls;
      }
  }


  private static Object getArrayElements (Object array, int[][] idx,
                                          int offset,
                                          int ndims, Class elemType)
  {
    if (offset >= ndims)
      {
        Object elem = Array.get (array, idx[offset][0]);
        if (offset < idx.length - 1)
          {
            return getArrayElements (elem, idx, offset + 1, ndims, elemType);
          }
        else
          {
            return elem;
          }
      }
    else
      {
        Class compType = elemType.getComponentType ();
        Object retval = Array.newInstance (compType, idx[offset].length);
        for (int i = 0; i < idx[offset].length; i++)
          {
            Object elem = Array.get (array, idx[offset][i]);
            if (offset < idx.length - 1)
              {
                elem = getArrayElements (elem, idx, offset + 1, ndims, compType);
              }
            Array.set (retval, i, elem);
          }
        return retval;
      }
  }


  public static Object arraySubsref (Object obj, int[][] idx)
    throws Exception
  {
    if (!obj.getClass ().isArray ())
      {
        throw new IllegalArgumentException ("not a Java array");
      }

    if (idx.length == 1)
      {
        if (idx[0].length == 1)
          {
            return Array.get (obj, idx[0][0]);
          }
        else
          {
            Object retval = Array.newInstance (obj.getClass ().
                                               getComponentType (),
                                               idx[0].length);
            for (int i = 0; i < idx[0].length; i++)
              {
                Array.set (retval, i, Array.get (obj, idx[0][i]));
              }
            return retval;
          }
      }
    else
      {
        int[] dims = new int[idx.length];
        for (int i = 0; i < idx.length; i++)
          {
            dims[i] = idx[i].length;
          }

        if (dims.length != getArrayClassNDims (obj.getClass ()))
          {
            throw new IllegalArgumentException ("index size mismatch");
          }

        /* resolve leading singletons */
        Object theObj = obj;
        int offset = 0;
        while (dims[offset] == 1)
          {
            theObj = Array.get (theObj, idx[offset][0]);
            offset = offset + 1;
            if (offset >= dims.length)
              {
                return theObj;
              }
          }
        if (offset > 0)
          {
            int[][] new_idx = new int[idx.length - offset][];
            System.arraycopy (idx, offset, new_idx, 0, idx.length - offset);
            return arraySubsref (theObj, new_idx);
          }

        /* chop trailing singletons */
        int ndims = dims.length;
        while (ndims > 1 && dims[ndims - 1] == 1)
          {
            ndims--;
          }

        /* create result array */
        Class elemClass = theObj.getClass ();
        for (int i = 0; i <= (dims.length - ndims); i++)
          {
            elemClass = elemClass.getComponentType ();
          }
        Object retval = Array.newInstance (elemClass, dims[0]);

        /* fill-in array */
        for (int i = 0; i < idx[0].length; i++)
          {
            Object elem = getArrayElements (Array.get (theObj, idx[0][i]),
                                            idx, 1, ndims, elemClass);
            Array.set (retval, i, elem);
          }

        return retval;
      }
  }


  private static Object setArrayElements (Object array, int[][] idx,
                                          int offset, int ndims, Object rhs)
    throws Exception
  {
    if (offset >= ndims)
      {
        if (offset < idx.length - 1)
          {
            setArrayElements (Array.get (array, idx[offset][0]), idx,
                              offset + 1, ndims, rhs);
          }
        else
          {
            Array.set (array, idx[offset][0], rhs);
          }
        return array;
      }
    else
      {
        for (int i = 0; i < idx[offset].length; i++)
          {
            if (offset < idx.length - 1)
              {
                setArrayElements (Array.get (array, idx[offset][i]), idx,
                                  offset + 1, ndims, Array.get (rhs, i));
              }
            else
              {
                Array.set (array, idx[offset][i], Array.get (rhs, i));
              }
          }
        return array;
      }
  }


  public static Object arraySubsasgn (Object obj, int[][] idx, Object rhs)
    throws Exception
  {
    if (!obj.getClass ().isArray ())
      {
        throw new IllegalArgumentException ("not a Java array");
      }

    if (idx.length == 1)
      {
        if (idx[0].length == 1)
          {
            Array.set (obj, idx[0][0], rhs);
          }
        else
          {
            for (int i = 0; i < idx[0].length; i++)
              {
                Array.set (obj, idx[0][i], Array.get (rhs, i));
              }
          }
        return obj;
      }
    else
      {
        int[] dims = new int[idx.length];
        for (int i = 0; i < idx.length; i++)
          {
            dims[i] = idx[i].length;
          }

        if (dims.length != getArrayClassNDims (obj.getClass ()))
          {
            throw new IllegalArgumentException ("index size mismatch");
          }

        /* resolve leading singletons */
        Object theObj = obj;
        int offset = 0;
        while (dims[offset] == 1 && offset < (dims.length - 1))
          {
            theObj = Array.get (theObj, idx[offset][0]);
            offset = offset + 1;
          }
        if (offset > 0)
          {
            int[][] new_idx = new int[idx.length - offset][];
            System.arraycopy (idx, offset, new_idx, 0, idx.length - offset);
            arraySubsasgn (theObj, new_idx, rhs);
            return obj;
          }

        /* chop trailing singletons */
        int ndims = dims.length;
        while (ndims > 1 && dims[ndims - 1] == 1)
          {
            ndims--;
          }

        for (int i = 0; i < idx[0].length; i++)
          {
            setArrayElements (Array.get (theObj, idx[0][i]), idx, 1, ndims,
                              Array.get (rhs, i));
          }

        return obj;
      }
  }


  public static Object createArray (Object cls, int[] dims)
    throws Exception
  {
    Class theClass;
    if (cls instanceof Class)
      {
        theClass = (Class) cls;
      }
    else if (cls instanceof String)
      {
        theClass = Class.forName ((String) cls, true, loader);
      }
    else
      {
        throw new IllegalArgumentException ("invalid class specification " +
                                            cls);
      }

    return Array.newInstance (theClass, dims);
  }


  public static Object createArray (Object cls, int length)
    throws Exception
  {
    return createArray (cls, new int[] {length});
  }
}
