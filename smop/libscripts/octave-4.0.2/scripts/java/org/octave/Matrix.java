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

import java.nio.*;
import java.text.DecimalFormat;

public class Matrix
{
  private int[] dims;
  private Buffer data;
  private Object cache = null;

  public Matrix ()
  {
    this (new double[0], new int[] {0, 0});
  }

  public Matrix (double[] data)
  {
    this (data, new int[] {1, data.length});
  }

  public Matrix (double[][] data)
  {
    int m = data.length;
    int n = (m > 0 ? data[0].length : 0);
    int idx = 0;
    double[] buf = new double[m*n];

    for (int j = 0; j < n; j++)
      for (int i = 0; i < m; i++)
        buf[idx++] = data[i][j];
    this.data = DoubleBuffer.wrap(buf);
    this.dims = new int[] {m, n};
    this.cache = data;
  }

  public Matrix (double[][][] data)
  {
    int m = data.length;
    int n = (m > 0 ? data[0].length : 0);
    int p = (n > 0 ? data[0][0].length : 0);
    int idx = 0;
    double[] buf = new double[m*n*p];

    for (int k = 0; k < p; k++)
      for (int j = 0; j < n; j++)
        for (int i = 0; i < m; i++)
          buf[idx++] = data[i][j][k];
    this.data = DoubleBuffer.wrap(buf);
    this.dims = new int[] {m, n, p};
    this.cache = data;
  }

  public Matrix (double[] data, int[] dims)
  {
    this.dims = dims;
    this.data = DoubleBuffer.wrap(data);
  }

  public Matrix (byte[] data, int[] dims)
  {
    this.dims = dims;
    this.data = ByteBuffer.wrap(data);
  }

  public Matrix (int[] data, int[] dims)
  {
    this.dims = dims;
    this.data = IntBuffer.wrap(data);
  }

  public double[] toDouble ()
  {
    if (data instanceof DoubleBuffer)
      return ((DoubleBuffer)data).array ();
    else
      throw new ClassCastException ("matrix is not of type `double'");
  }

  public byte[] toByte ()
  {
    if (data instanceof ByteBuffer)
      return ((ByteBuffer)data).array ();
    else
      throw new ClassCastException ("matrix is not of type `byte'");
  }

  public int[] toInt ()
  {
    if (data instanceof IntBuffer)
      return ((IntBuffer)data).array ();
    else
      throw new ClassCastException ("matrix is not of type `integer'");
  }

  public int getNDims ()
  {
    return (dims == null ? 0 : dims.length);
  }

  public int getDim (int index)
  {
    return (dims == null || index < 0 || index >= dims.length ? -1 : dims[index]);
  }

  public int[] getDims ()
  {
    return dims;
  }

  public String getClassName ()
  {
    if (data instanceof DoubleBuffer)
      return "double";
    else if (data instanceof IntBuffer)
      return "integer";
    else if (data instanceof ByteBuffer)
      return "byte";
    else
      return "unknown";
  }

  public String toString ()
  {
    if (dims == null || data == null)
      return "null";

    String s = "";

    if (dims.length == 2 && dims[0] == 1 && dims[1] <= 5)
      {
        if (data instanceof DoubleBuffer)
          {
            DoubleBuffer b = (DoubleBuffer)data;
            DecimalFormat fmt = new DecimalFormat ("0.0000 ");
            for (int i = 0; i < b.capacity (); i++)
              s += fmt.format (b.get (i));
          }
        else if (data instanceof IntBuffer)
          {
            IntBuffer b = (IntBuffer)data;
            for (int i = 0; i < b.capacity (); i++)
              s += (Integer.toString (b.get (i)) + " ");
          }
        else if (data instanceof ByteBuffer)
          {
            ByteBuffer b = (ByteBuffer)data;
            for (int i = 0; i < b.capacity (); i++)
              s += (Byte.toString (b.get (i)) + " ");
          }
        s = ("[ " + s + "]");
      }
    else if (dims.length == 2 && dims[0] == 0 && dims[1] == 0)
      s = "[ ]";
    else
      {
        for (int i = 0; i < dims.length; i++)
          if (i == 0)
            s = Integer.toString (dims[i]);
          else
            s += (" by " + Integer.toString (dims[i]));
        s = ("[ (" + s + ") array of " + getClassName () + " ]");
      }

    return s;
  }

  public static Object ident (Object o)
  {
    System.out.println (o);
    return o;
  }

  public boolean equals (Object value)
  {
    if (value instanceof Matrix)
      {
        Matrix m = (Matrix)value;
        if (!java.util.Arrays.equals (dims, m.dims))
          return false;
        return data.equals (m.data);
      }
    else
      return false;
  }

  public boolean isEmpty ()
  {
    return (data == null || dims == null || data.capacity () == 0);
  }

  public boolean isVector ()
  {
    return (dims.length == 1 ||
            (dims.length == 2 && (dims[0] == 1 || dims[1] == 1 ||
                                  (dims[0] == 0 && dims[1] == 0))));
  }

  public int getLength ()
  {
    return data.capacity ();
  }

  public double[] asDoubleVector ()
  {
    if (data instanceof DoubleBuffer)
      return toDouble ();
    else
      System.out.println ("Warning: invalid conversion to double vector");
    return null;
  }

  public double[][] asDoubleMatrix ()
  {
    if (cache != null)
      {
        try { return (double[][])cache; }
        catch (ClassCastException e) { }
      }

    if (data instanceof DoubleBuffer && dims.length == 2)
      {
        double[][] m = new double[dims[0]][dims[1]];
        double[] data = ((DoubleBuffer)this.data).array ();
        int idx = 0;
        if (data.length > 0)
          for (int j = 0; j < m[0].length; j++)
            for (int i = 0; i < m.length; i++)
              m[i][j] = data[idx++];
        cache = m;
        return m;
      }
    else
      System.out.println ("Warning: invalid conversion to double matrix");

    return null;
  }

  public double[][][] asDoubleMatrix3 ()
  {
    if (cache != null)
      {
        try { return (double[][][])cache; }
        catch (ClassCastException e) { }
      }

    if (data instanceof DoubleBuffer && dims.length == 3)
      {
        double[][][] m = new double[dims[0]][dims[1]][dims[2]];
        double[] data = ((DoubleBuffer)this.data).array ();
        int idx = 0;
        if (data.length > 0)
          for (int k = 0; k < dims[2]; k++)
            for (int j = 0; j < dims[1]; j++)
              for (int i = 0; i < dims[0]; i++)
                m[i][j][k] = data[idx++];
        cache = m;
        return m;
      }
    else
      System.out.println ("Warning: invalid conversion to double array");

    return null;
  }

  public int[][] asIntMatrix ()
  {
    if (cache != null)
      {
        try { return (int[][])cache; }
        catch (ClassCastException e) { }
      }

    if (data instanceof IntBuffer && dims.length == 2)
      {
        int[][] m = new int[dims[0]][dims[1]];
        int[] data = ((IntBuffer)this.data).array ();
        int idx = 0;
        if (data.length > 0)
          for (int j = 0; j < m[0].length; j++)
            for (int i = 0; i < m.length; i++)
              m[i][j] = data[idx++];
        cache = m;
        return m;
      }
    else
      System.out.println ("Warning: invalid conversion to integer matrix");

    return null;
  }

  public double minValue ()
  {
    double val = Double.POSITIVE_INFINITY;

    if (data instanceof DoubleBuffer)
      {
        double[] buf = ((DoubleBuffer)data).array ();
        for (int i = 0; i < buf.length; i++)
          if (buf[i] < val)
            val = buf[i];
      }
    else if (data instanceof ByteBuffer)
      {
        byte[] buf = ((ByteBuffer)data).array ();
        for (int i = 0; i < buf.length; i++)
          if (buf[i] < val)
            val = buf[i];
      }
    else
      System.out.println ("Warning: cannot compute min value for array of type `" + getClassName () + "'");

    return val;
  }

  public double maxValue ()
  {
    double val = Double.NEGATIVE_INFINITY;

    if (data instanceof DoubleBuffer)
      {
        double[] buf = ((DoubleBuffer)data).array ();
        for (int i = 0; i < buf.length; i++)
          if (buf[i] > val)
            val = buf[i];
      }
    else if (data instanceof ByteBuffer)
      {
        byte[] buf = ((ByteBuffer)data).array ();
        for (int i = 0; i <buf.length; i++)
          if (buf[i] > val)
            val = buf[i];
      }
    else
      System.out.println ("Warning: cannot compute max value for array of type `" + getClassName () + "'");

    return val;
  }
}
