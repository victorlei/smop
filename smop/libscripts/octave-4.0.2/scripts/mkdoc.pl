#! /usr/bin/perl
use utf8;

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

use strict;
use warnings;
use File::Spec;
use Cwd;

## Expecting arguments in this order:
##
##  SRCDIR SRCDIR-FILES ... -- LOCAL-FILES ...

unless (@ARGV >= 2) { die "Usage: $0 srcdir m_filename1 ..." ; }

my $srcdir = shift (@ARGV);

print <<__END_OF_MSG__;
### DO NOT EDIT!
###
### This file is generated automatically from Octave source files.
### Edit source files directly and run make to update this file.

__END_OF_MSG__

MFILE: foreach my $m_fname (@ARGV)
{
  if ($m_fname eq "--")
    {
      $srcdir = getcwd ();
      next MFILE;
    }

  my $full_fname = File::Spec->catfile ($srcdir, $m_fname);
  my @paths = File::Spec->splitdir ($full_fname);
  if (@paths < 3
      || $paths[-2] eq "private"   # skip private directories
      || $paths[-1] !~ s/\.m$//i)  # skip non m-files, and remove extension
    { next MFILE; }

  ## @classes will have @class/method as their function name
  my $fcn = $paths[-2] =~ m/^@/ ? File::Spec->catfile (@paths[-2, -1])
                                : $paths[-1];

  my @help_txt = gethelp ($fcn, $full_fname);
  next MFILE unless @help_txt;

  print "\x{1d}$fcn\n";
  print "\@c $fcn ", File::Spec->catfile ("scripts", $m_fname), "\n";

  foreach $_ (@help_txt)
    {
      my $in_example = (m/\s*\@example\b/ .. m/\s*\@end\s+example\b/);
      s/^\s+\@/\@/ unless $in_example;
      s/^\s+(\@(?:end)\s+(group|example))/$1/;
      print $_;
    }
}

################################################################################
# Subroutines
################################################################################
sub gethelp
{
  my $fcn   = shift;
  my $fname = shift;
  open (my $fh, "<", $fname) or return;

  my @help_txt;
  while (my $line = <$fh>)
    {
      next if $line =~ m/^\s*$/;      # skip empty lines
      last if $line !~ m/^\s*(#|%)/;  # out of here once code starts

      my $reading_block = sub {defined ($line = <$fh>) && $line !~ m/^\s*$/};

      ## Skip this block
      if ($line =~ /(Copyright|Author)/)
        { while (&$reading_block ()) {} }
      else
        {
          do
            {
              $line =~ s/^\s*(%|#)+ ?//;
              push (@help_txt, $line);
            } while (&$reading_block ());
          last;
        }
    }

  close ($fh);
  return @help_txt;
}
