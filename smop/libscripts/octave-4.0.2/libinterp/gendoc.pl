#! /usr/bin/perl -w
#
# Copyright (C) 2012-2015 Rik Wehbring
#
# This file is part of Octave.
#
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.

unless (@ARGV > 1) { die "Usage: $0 df-file1 ..." }

print <<__END_OF_MSG__;
### DO NOT EDIT!
###
### This file is generated automatically from Octave source files.
### Edit source files directly and run make to update this file.

__END_OF_MSG__

DFFILE: foreach $df_fname (@ARGV)
{
  open (DF_FH, $df_fname) or die "Unable to open $df_fname";

  $src_fname = "";
  @func_list = ();
  @docstr = ();

  LINE: while (<DF_FH>)
  {
    if (/XDEFUN_FILE_NAME \("([^"]+)"/)
    {
      $src_fname = $1;
      next LINE;
    }
    if (/XDEF/ and ! /XDEFALIAS/)
    {
      ## Decode 4 or 5 part macro definition.
      ($func, $str) = /\("?(\w+)"?,[^,]+,[^,]+,(?:[^,]+,)?\s*"(.*)"\)\s*$/ ;

      unless ($func) { die "Unable to parse $df_fname at line $.\n" }

      push (@func_list, $func);
      ## Do escape sequence expansion
      $str =~ s/(?<!\\)\\n/\n/g;
      $str =~ s/\\([^\\])/$1/g;
      $str =~ s/\\\\/\\/g;
      push (@docstr, $str);
    }
  }
  close (DF_FH);

  ## Print results in DOCSTRING format
  foreach $i (0 .. $#func_list)
  {
    $func = $func_list[$i];
    print "\x{1d}$func\n";
    print "\@c $func $src_fname\n";
    print $docstr[$i],"\n";
  }

}

