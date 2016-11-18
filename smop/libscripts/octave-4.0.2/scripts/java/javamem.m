## Copyright (C) 2010, 2013 Philip Nienhuis
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {} javamem ()
## @deftypefnx {Function File} {@var{jmem} =} javamem ()
## Show the current memory usage of the Java virtual machine (JVM) and run the
## garbage collector.
##
## When no return argument is given the info is printed to the screen.
## Otherwise, the output cell array @var{jmem} contains Maximum, Total, and
## Free memory (in bytes).
##
## All Java-based routines are run in the JVM's shared memory pool, a
## dedicated and separate part of memory claimed by the JVM from your
## computer's total memory (which comprises physical RAM and virtual memory /
## swap space on hard disk).
##
## The maximum allowable memory usage can be configured using the file
## @file{java.opts}.  The directory where this file resides is determined by
## the environment variable @w{@env{OCTAVE_JAVA_DIR}}.  If unset, the directory
## where @file{javaaddpath.m} resides is used instead (typically
## @file{@w{@env{OCTAVE_HOME}}/share/octave/@w{@env{OCTAVE_VERSION}}/m/java/}).
##
## @file{java.opts} is a plain text file with one option per line.  The default
## initial memory size and default maximum memory size (which are both system
## dependent) can be overridden like so:
##
## @nospell{-Xms64m}
##
## @nospell{-Xmx512m}
##
## (in megabytes in this example).
## You can adapt these values to your own requirements if your system has
## limited available physical memory or if you get Java memory errors.
##
## @qcode{"Total memory"} is what the operating system has currently assigned
## to the JVM and depends on actual and active memory usage.
## @qcode{"Free memory"} is self-explanatory.  During operation of Java-based
## Octave functions the amount of Total and Free memory will vary, due to
## Java's own cleaning up and your operating system's memory management.
## @end deftypefn

## Author: Philip Nienhuis
## Created: 2010-03-25
## Updates:
## 2010-03-26 Changed name to javamem & indentation to double spaces
## 2010-08-25 Corrected text on java memory assignments
## 2010-09-05 Further overhauled help text

function jmem = javamem ()

  rt = javaMethod ("getRuntime", "java.lang.Runtime");
  rt.gc;
  jvmem = cell (3, 1);
  jvmem{1} = rt.maxMemory ();
  jvmem{2} = rt.totalMemory ();
  jvmem{3} = rt.freeMemory ();

  if (nargout == 0)
    printf ("\nJava virtual machine (JVM) memory info:\n");
    printf ("Maximum available memory:        %5d MiB;\n",
            jvmem{1} / 1024 / 1024);
    printf ("   (...running garbage collector...)\n");
    printf ("OK, current status:\n");
    printf ("Total memory in virtual machine: %5d MiB;\n",
            jvmem{2} / 1024 / 1024);
    printf ("Free memory in virtual machine:  %5d MiB;\n",
            jvmem{3} / 1024 / 1024);
    printf ("%d CPUs available.\n", rt.availableProcessors ());
  else
    jmem = jvmem;
  endif

endfunction

