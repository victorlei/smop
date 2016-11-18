#! /usr/bin/perl -w
#
# Copyright (C) 2002-2015 John W. Eaton
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

# Generate option handling code from a simpler input files for
# Octave's functions like lsode, dassl, etc.

# FIXME:
#
# * Improve default documentation and/or individual documentation
#   in data files.
#
# * Fix print/show code to display/return something more informative
#   for special values (for example, -1 ==> infinite in some cases).
#   Probably need more information in the data files for this.

# Input file format:
#
# CLASS = string
# FCN_NAME = string
# INCLUDE = file
# DOC_STRING doc END_DOC_STRING
# OPTION
#   NAME = string
#   DOC_ITEM doc END_DOC_ITEM
#   TYPE = string
#   SET_ARG_TYPE = string   (optional, defaults to TYPE)
#   INIT_VALUE = string | INIT_BODY code END_INIT_BODY
#   SET_EXPR = string | SET_BODY code END_SET_BODY | SET_CODE code END_SET_CODE
# END_OPTION
#
# END_* must appear at beginning of line (whitespace ignored).

################################################################################
# Load packages to
# 1) process command line options
################################################################################
use Getopt::Long;

################################################################################
# Extract command line arguments
&parse_options;

$DEFN_FILE = shift @ARGV;
open (DEFN_FILE) or die "unable to open input definition file $DEFN_FILE";

################################################################################
# Initialize variables
$BLANK_LINE = qr/^\s*$/;
$COMMENT = qr/^\s*#/;

################################################################################
# Process file
$OPT_NUM = 0;

&parse_input;

&process_data;

# Produce desired style of output
&emit_opt_class_header if $opt_class_header;
&emit_opt_handler_fcns if $opt_handler_fcns;
&emit_options_debug if $opt_debug;

# End of main code

################################################################################
# Subroutines
################################################################################

sub parse_input
{
  LINE: while (<DEFN_FILE>)
    {
      next LINE if /$BLANK_LINE/;
      next LINE if /$COMMENT/;

      if (/^\s*OPTION\s*$/)
        {
          &parse_option_block;
        }
      elsif (/^\s*CLASS\s*=\s*"(\w+)"\s*$/)
        {
          die "duplicate CLASS" if defined $CLASS;
          $CLASS = $1;
          $CLASS_NAME = "${CLASS}_options";
          $STRUCT_NAME = "${CLASS_NAME}_struct";
          $STATIC_TABLE_NAME = "${CLASS_NAME}_table";
        }
      elsif (/^\s*FCN_NAME\s*=\s*"(\w+)"\s*$/)
        {
          die "duplicate FCN_NAME" if defined $FCN_NAME;
          $FCN_NAME = $1;
        }
      elsif (/^\s*INCLUDE\s*=\s*"(\S+)"\s*$/)
        {
          $INCLUDE .= "#include <$1>\n";
        }
      elsif (/^\s*DOC_STRING\s*$/)
        {
          die "duplicate DOC_STRING" if defined $DOC_STRING;
          while (defined ($_ = <DEFN_FILE>) and not /^\s*END_DOC_STRING\s*$/)
          {
            $DOC_STRING .= $_;
          }
          $DOC_STRING =~ s/\n/\\n\\\n/g;
        }
      else
        {
          die "mk-opts.pl: unknown command: $_\n"
        }
    }
  $INCLUDE = "" if not defined $INCLUDE;   # Initialize value if required
}

sub parse_option_block
{
  while (<DEFN_FILE>)
    {
      next if /$BLANK_LINE/;

      die "missing END_OPTION" if /^\s*OPTION\s*$/;

      last if /^\s*END_OPTION\s*$/;

      if (/^\s*NAME\s*=\s*"(.*)"\s*$/)
        {
          die "duplicate NAME" if defined $NAME[$OPT_NUM];
          $NAME[$OPT_NUM] = $1;
          ($OPT[$OPT_NUM] = $NAME[$OPT_NUM]) =~ s/\s+/_/g;
          $OPTVAR[$OPT_NUM] = 'x_' . $OPT[$OPT_NUM];
          $KW_TOK[$OPT_NUM] = [ split (' ', $NAME[$OPT_NUM]) ];
          $N_TOKS[$OPT_NUM] = @{$KW_TOK[$OPT_NUM]};
        }
      elsif (/^\s*DOC_ITEM\s*$/)
        {
          die "duplicate DOC_ITEM" if defined $DOC_ITEM[$OPT_NUM];
          while (defined ($_ = <DEFN_FILE>) and not /^\s*END_DOC_ITEM\s*$/)
          {
            $DOC_ITEM[$OPT_NUM] .= $_;
          }
          $DOC_ITEM[$OPT_NUM] =~ s/\n/\\n\\\n/g;
        }
      elsif (/^\s*TYPE\s*=\s*"(.*)"\s*$/)
        {
          die "duplicate TYPE" if defined $TYPE[$OPT_NUM];
          $TYPE[$OPT_NUM] = $1;
        }
      elsif (/^\s*SET_ARG_TYPE\s*=\s*"(.*)"\s*$/)
        {
          die "duplicate SET_ARG_TYPE" if defined $SET_ARG_TYPE[$OPT_NUM];
          $SET_ARG_TYPE[$OPT_NUM] = $1;
        }
      elsif (/^\s*INIT_VALUE\s*=\s*"(.*)"\s*$/)
        {
          die "duplicate INIT_VALUE" if defined $INIT_VALUE[$OPT_NUM];
          $INIT_VALUE[$OPT_NUM] = $1;
        }
      elsif (/^\s*SET_EXPR\s*=\s*"(.*)"\s*$/)
        {
          die "duplicate SET_EXPR" if defined $SET_EXPR[$OPT_NUM];
          $SET_EXPR[$OPT_NUM] = $1;
        }
      elsif (/^\s*INIT_BODY\s*$/)
        {
          die "duplicate INIT_BODY" if defined $INIT_BODY[$OPT_NUM];
          while (defined ($_ = <DEFN_FILE>) and not /^\s*END_INIT_BODY\s*$/)
          {
            $INIT_BODY[$OPT_NUM] .= $_;
          }
        }
      elsif (/^\s*SET_BODY\s*$/)
        {
          die "duplicate SET_BODY" if defined $INIT_BODY[$OPT_NUM];
          while (defined ($_ = <DEFN_FILE>) and not /^\s*END_SET_BODY\s*$/)
          {
            $SET_BODY[$OPT_NUM] .= $_;
          }
        }
      elsif (/^\s*SET_CODE\s*$/)
        {
          die "duplicate SET_CODE" if defined $SET_CODE[$OPT_NUM];
          while (defined ($_ = <DEFN_FILE>) and not /^\s*END_SET_CODE\s*$/)
          {
            $SET_CODE[$OPT_NUM] .= $_;
          }
        }
    }

  if (not defined $SET_ARG_TYPE[$OPT_NUM])
    {
      $SET_ARG_TYPE[$OPT_NUM] = $TYPE[$OPT_NUM];
    }
  else
    {
      $SET_ARG_TYPE[$OPT_NUM]
        = substopt ($SET_ARG_TYPE[$OPT_NUM], $OPTVAR[$OPT_NUM],
                    $OPT[$OPT_NUM], $TYPE[$OPT_NUM]);
    }

  $OPT_NUM++;
}

sub process_data
{
  $MAX_TOKENS = max (@N_TOKS);

  &get_min_match_len_info;

  $FCN_NAME = lc ($CLASS) if not defined $FCN_NAME;

  $OPT_FCN_NAME = "${FCN_NAME}_options" if not defined $OPT_FCN_NAME;

  $STATIC_OBJECT_NAME = "${FCN_NAME}_opts";

  if (not defined $DOC_STRING)
    {
      $DOC_STRING = "Query or set options for the function \@code{$FCN_NAME}.\\n\\
\\n\\
When called with no arguments, the names of all available options and\\n\\
their current values are displayed.\\n\\
\\n\\
Given one argument, return the value of the option \@var{opt}.\\n\\
\\n\\
When called with two arguments, \@code{$OPT_FCN_NAME} sets the option\\n\\
\@var{opt} to value \@var{val}.";
    }
}

## FIXME: What does this routine do?  And can it be simpler to understand?
sub get_min_match_len_info
{
  my ($i, $j, $k);

  for ($i = 0; $i < $OPT_NUM; $i++)
    {
      for ($j = 0; $j < $MAX_TOKENS; $j++)
        {
          $MIN_TOK_LEN_TO_MATCH[$i][$j] = 0;
        }

      $MIN_TOKS_TO_MATCH[$i] = 1;

    L1: for ($k = 0; $k < $OPT_NUM; $k++)
        {
          my $duplicate = 1;

          if ($i != $k)
            {
            L2: for ($j = 0; $j < $MAX_TOKENS; $j++)
                {
                  if ($j < $N_TOKS[$i])
                    {
                      if ($KW_TOK[$i][$j] eq $KW_TOK[$k][$j])
                        {
                          if ($MIN_TOK_LEN_TO_MATCH[$i][$j] == 0)
                            {
                              $MIN_TOK_LEN_TO_MATCH[$i][$j] = 1;
                            }

                          $MIN_TOKS_TO_MATCH[$i]++;
                        }
                      else
                        {
                          $duplicate = 0;

                          if ($MIN_TOK_LEN_TO_MATCH[$i][$j] == 0)
                            {
                              $MIN_TOK_LEN_TO_MATCH[$i][$j] = 1;
                            }

                          my @s = split (//, $KW_TOK[$i][$j]);
                          my @t = split (//, $KW_TOK[$k][$j]);

                          my ($n, $ii);
                          $n = scalar (@s);
                          $n = scalar (@t) if (@t < $n);

                          for ($ii = 0; $ii < $n; $ii++)
                            {
                              if ("$s[$ii]" eq "$t[$ii]")
                                {
                                  if ($ii + 2 > $MIN_TOK_LEN_TO_MATCH[$i][$j])
                                    {
                                      $MIN_TOK_LEN_TO_MATCH[$i][$j]++;
                                    }
                                }
                              else
                                {
                                  last L2;
                                }
                            }

                          last L1;
                        }
                    }
                  else
                    {
                      die qq|ambiguous options "$NAME[$i]" and "$NAME[$k]"| if $duplicate;
                    }
                }
            }
        }
    }
}  # end of get_min_match_len_info


sub emit_copy_body
{
  my ($pfx, $var) = @_;

  for (my $i = 0; $i < $OPT_NUM; $i++)
    {
      print "${pfx}$OPTVAR[$i] = ${var}.$OPTVAR[$i];\n";
    }

  print "${pfx}reset = ${var}.reset;\n";
}

## To silence GCC warnings, we create an initialization list even
## though the init function actually does the work of initialization.

sub emit_default_init_list
{
  my ($prefix) = @_;

  print "$OPTVAR[0] (),\n" unless ($OPT_NUM == 0);

  for (my $i = 1; $i < $OPT_NUM; $i++)
    {
      print "${prefix}$OPTVAR[$i] (),\n";
    }

  print "${prefix}reset ()\n";
}

sub emit_copy_ctor_init_list
{
  my ($prefix, $var) = @_;

  print "$OPTVAR[0] ($var.$OPTVAR[0]),\n" unless ($OPT_NUM == 0);

  for (my $i = 1; $i < $OPT_NUM; $i++)
    {
      print "${prefix}$OPTVAR[$i] ($var.$OPTVAR[$i]),\n";
    }

  print "${prefix}reset ($var.reset)\n";
}

sub emit_opt_class_header
{
  my ($i, $s);

  print <<"_END_EMIT_OPT_CLASS_HEADER_";
// DO NOT EDIT!
// Generated automatically from $DEFN_FILE.

#if !defined (octave_${CLASS_NAME}_h)
#define octave_${CLASS_NAME}_h 1

#include <cfloat>
#include <cmath>

$INCLUDE

class
$CLASS_NAME
{
public:

  $CLASS_NAME (void)
_END_EMIT_OPT_CLASS_HEADER_

  print '    : ';
  emit_default_init_list ("      ");

  print "    {
      init ();
    }

  $CLASS_NAME (const ${CLASS_NAME}& opt)
    : ";

  emit_copy_ctor_init_list ("      ", "opt");

  print "    { }

  ${CLASS_NAME}& operator = (const ${CLASS_NAME}& opt)
    {
      if (this != &opt)
        {\n";

  emit_copy_body ('          ', 'opt');

  print "        }

      return *this;
    }

  ~$CLASS_NAME (void) { }\n";

  print "\n  void init (void)\n    {\n";

  for ($i = 0; $i < $OPT_NUM; $i++)
    {
      if ($INIT_VALUE[$i])
        {
          print "      $OPTVAR[$i] = $INIT_VALUE[$i];\n";
        }
      elsif ($INIT_BODY[$i])
        {
          $s = substopt ($INIT_BODY[$i], $OPTVAR[$i], $OPT[$i], $TYPE[$i]);
          chomp ($s);
          $s =~ s/^\s*/      /g;
          $s =~ s/\n\s*/\n      /g;
          print $s,"\n";
        }
    }

  print "      reset = true;\n",
        "    }\n";

  ## For backward compatibility and because set_options is probably
  ## a better name in some contexts:

  print "\n  void set_options (const ${CLASS_NAME}& opt)\n",
        "    {\n";

  emit_copy_body ('      ', 'opt');

  print "    }\n\n  void set_default_options (void) { init (); }\n";

  for ($i = 0; $i < $OPT_NUM; $i++)
    {
      if ($SET_EXPR[$i])
        {
          emit_set_decl ($i);

          print "\n    { $OPTVAR[$i] = $SET_EXPR[$i]; reset = true; }\n";
        }
      elsif ($SET_BODY[$i])
        {
          emit_set_decl ($i);

          $s = substopt ($SET_BODY[$i], $OPTVAR[$i], $OPT[$i], $TYPE[$i]);
          chomp ($s);
          $s = '  ' . $s;
          $s =~ s/\n/\n  /g;
          print "\n    {\n$s\n      reset = true;\n    }\n";
        }
      elsif ($SET_CODE[$i])
        {
          $s = substopt ($SET_CODE[$i], $OPTVAR[$i], $OPT[$i], $TYPE[$i]);
          chomp ($s);
          $s =~ s/^  //g;
          $s =~ s/\n  /\n/g;
          print "\n",$s,"\n";
        }
    }

  for ($i = 0; $i < $OPT_NUM; $i++)
    {
      print "  $TYPE[$i] $OPT[$i] (void) const\n    { return $OPTVAR[$i]; }\n\n";
    }

  print "private:\n\n";

  for ($i = 0; $i < $OPT_NUM; $i++)
    {
      print "  $TYPE[$i] $OPTVAR[$i];\n";
    }

  print "\nprotected:\n\n  bool reset;\n};\n\n#endif\n";
}

sub emit_set_decl
{
  my ($i) = @_;

  print "\n  void set_$OPT[$i] ($SET_ARG_TYPE[$i] val)";
}

sub emit_opt_handler_fcns
{
  my $header = $DEFN_FILE;
  $header =~ s/[.]\w*$/.h/;      # replace .in with .h
  $header =~ s|^.*/([^/]*)$|$1|; # strip directory part

  print <<"_END_EMIT_OPT_HANDLER_FCNS_";
// DO NOT EDIT!
// Generated automatically from $DEFN_FILE.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iomanip>
#include <iostream>

#include "$header"

#include "defun.h"
#include "pr-output.h"

#include "oct-obj.h"
#include "utils.h"
#include "pager.h"

static $CLASS_NAME $STATIC_OBJECT_NAME;

_END_EMIT_OPT_HANDLER_FCNS_

  &emit_struct_decl;

  &emit_struct_def;

  &emit_print_function;

  &emit_set_functions;

  &emit_show_function;

  &emit_options_function;
}

sub emit_struct_decl
{
  print <<"_END_PRINT_STRUCT_DECL_";
#define MAX_TOKENS $MAX_TOKENS

struct $STRUCT_NAME
{
  const char *keyword;
  const char *kw_tok[MAX_TOKENS + 1];
  int min_len[MAX_TOKENS + 1];
  int min_toks_to_match;
};

_END_PRINT_STRUCT_DECL_
}

sub emit_struct_def
{
  my $i;

  print "#define NUM_OPTIONS $OPT_NUM\n\n";

  print "static $STRUCT_NAME $STATIC_TABLE_NAME [] =\n{\n";

  for ($i = 0; $i < ($OPT_NUM - 1); $i++)
    {
      emit_option_table_entry ($i, 0);
      print "\n";
    }
  emit_option_table_entry ($i, 0);

  print "};\n\n";
}

sub emit_option_table_entry
{
  my ($i, $empty) = @_;

  my $k;

  if ($empty)
    {
      print "  { 0,\n";
    }
  else
    {
      print "  { \"$NAME[$i]\",\n";
    }

  my $n = scalar $#{$KW_TOK[$i]};
  print "    {";
  for $k (0 .. $MAX_TOKENS)
    {
      if ($empty or $k > $n)
        {
          print " 0,";
        }
      else
        {
          print " \"$KW_TOK[$i][$k]\",";
        }
    }
  print " },\n";

  print "    {";
  for $k (0 .. $MAX_TOKENS)
    {
      if ($empty or $k > $n)
        {
          print " 0,";
        }
      else
        {
          print " $MIN_TOK_LEN_TO_MATCH[$i][$k],";
        }
    }
  print " }, $MIN_TOKS_TO_MATCH[$i], ";

  print "},\n";
}

sub emit_print_function
{
  ## FIXME -- determine the width of the table automatically.

  print qq|static void
print_$CLASS_NAME (std::ostream& os)
{
  std::ostringstream buf;

  os << "\\n"
     << "Options for $CLASS include:\\n\\n"
     << "  keyword                                             value\\n"
     << "  -------                                             -----\\n";

  $STRUCT_NAME *list = $STATIC_TABLE_NAME;\n\n|;

  for (my $i = 0; $i < $OPT_NUM; $i++)
    {
      print qq|  {\n    os << "  "
        << std::setiosflags (std::ios::left) << std::setw (50)
        << list[$i].keyword
        << std::resetiosflags (std::ios::left)
        << "  ";\n\n|;

      if ($TYPE[$i] eq "double")
        {
          print qq|    double val = $STATIC_OBJECT_NAME.$OPT[$i] ();\n\n|;
          print qq|    os << val << "\\n";\n|;
        }
      elsif ($TYPE[$i] eq "float")
        {
          print qq|    float val = $STATIC_OBJECT_NAME.$OPT[$i] ();\n\n|;
          print qq|    os << val << "\\n";\n|;
        }
      elsif ($TYPE[$i] eq "int" or $TYPE[$i] eq "octave_idx_type")
        {
          print qq|    int val = $STATIC_OBJECT_NAME.$OPT[$i] ();\n\n|;
          print qq|    os << val << "\\n";\n|;
        }
      elsif ($TYPE[$i] eq "std::string")
        {
          print qq|    os << $STATIC_OBJECT_NAME.$OPT[$i] () << "\\n";\n|;
        }
      elsif ($TYPE[$i] eq "Array<int>" or $TYPE[$i] eq "Array<octave_idx_type>")
        {
          my $elt_type;
          if ($TYPE[$i] eq "Array<int>")
            {
              $elt_type = "int";
            }
          else
            {
              $elt_type = "octave_idx_type";
            }
          print qq|    Array<$elt_type> val = $STATIC_OBJECT_NAME.$OPT[$i] ();\n\n|;
          print qq|    if (val.length () == 1)
      {
        os << val(0) << "\\n";
      }
    else
      {
        os << "\\n\\n";
        octave_idx_type len = val.length ();
        Matrix tmp (len, 1);
        for (octave_idx_type i = 0; i < len; i++)
          tmp(i,0) = val(i);
        octave_print_internal (os, tmp, false, 2);
        os << "\\n\\n";
      }\n|;
        }
      elsif ($TYPE[$i] eq "Array<double>")
        {
          print qq|    Array<double> val = $STATIC_OBJECT_NAME.$OPT[$i] ();\n\n|;
          print qq|    if (val.length () == 1)
      {
        os << val(0) << "\\n";
      }
    else
      {
        os << "\\n\\n";
        Matrix tmp = Matrix (ColumnVector (val));
        octave_print_internal (os, tmp, false, 2);
        os << "\\n\\n";
      }\n|;
        }
      elsif ($TYPE[$i] eq "Array<float>")
        {
          print qq|    Array<float> val = $STATIC_OBJECT_NAME.$OPT[$i] ();\n\n|;
          print qq|    if (val.length () == 1)
      {
        os << val(0) << "\\n";
      }
    else
      {
        os << "\\n\\n";
        FloatMatrix tmp = FloatMatrix (FloatColumnVector (val));
        octave_print_internal (os, tmp, false, 2);
        os << "\\n\\n";
      }\n|;
        }
      else
        {
          die ("unknown type $TYPE[$i]");
        }

      print "  }\n\n";
    }

  print qq|  os << "\\n";\n}\n\n|;
}

sub emit_set_functions
{
  print "static void
set_$CLASS_NAME (const std::string& keyword, const octave_value& val)
{
  $STRUCT_NAME *list = $STATIC_TABLE_NAME;\n\n";

  my $iftok = "if";

  for (my $i = 0; $i < $OPT_NUM; $i++)
    {
      $iftok = "else if" if ($i > 0);

      print "  $iftok (keyword_almost_match (list[$i].kw_tok, list[$i].min_len,
           keyword, list[$i].min_toks_to_match, MAX_TOKENS))
    {\n";

      if ($TYPE[$i] eq "double")
        {
          print "      double tmp = val.double_value ();\n\n";
          print "      if (! error_state)
        $STATIC_OBJECT_NAME.set_$OPT[$i] (tmp);\n";
        }
      elsif ($TYPE[$i] eq "float")
        {
          print "      float tmp = val.float_value ();\n\n";
          print "      if (! error_state)
        $STATIC_OBJECT_NAME.set_$OPT[$i] (tmp);\n";
        }
      elsif ($TYPE[$i] eq "int" or $TYPE[$i] eq "octave_idx_type")
        {
          print "      int tmp = val.int_value ();\n\n";
          print "      if (! error_state)
        $STATIC_OBJECT_NAME.set_$OPT[$i] (tmp);\n";
        }
      elsif ($TYPE[$i] eq "std::string")
        {
          print "      std::string tmp = val.string_value ();\n\n";
          print "      if (! error_state)
        $STATIC_OBJECT_NAME.set_$OPT[$i] (tmp);\n";
        }
      elsif ($TYPE[$i] eq "Array<int>" or $TYPE[$i] eq "Array<octave_idx_type>")
        {
          print "      Array<int> tmp = val.int_vector_value ();\n\n";
          print "      if (! error_state)
        $STATIC_OBJECT_NAME.set_$OPT[$i] (tmp);\n";
        }
      elsif ($TYPE[$i] eq "Array<double>")
        {
          print "      Array<double> tmp = val.vector_value ();\n\n";
          print "      if (! error_state)
        $STATIC_OBJECT_NAME.set_$OPT[$i] (tmp);\n";
        }
      elsif ($TYPE[$i] eq "Array<float>")
        {
          print "      Array<float> tmp = val.float_vector_value ();\n\n";
          print "      if (! error_state)
        $STATIC_OBJECT_NAME.set_$OPT[$i] (tmp);\n";
        }
      else
        {
          die ("unknown type $TYPE[$i]");
        }

      print "    }\n";
    }

  print qq|  else
    {
      warning ("$OPT_FCN_NAME: no match for `%s'", keyword.c_str ());
    }
}\n\n|;
}

sub emit_show_function
{
  print "static octave_value_list
show_$CLASS_NAME (const std::string& keyword)
{
  octave_value retval;

  $STRUCT_NAME *list = $STATIC_TABLE_NAME;\n\n";

  my $iftok = "if";

  for (my $i = 0; $i < $OPT_NUM; $i++)
    {
      $iftok = "else if" if ($i > 0);

      print "  $iftok (keyword_almost_match (list[$i].kw_tok, list[$i].min_len,
           keyword, list[$i].min_toks_to_match, MAX_TOKENS))
    {\n";

      if ($TYPE[$i] eq "double")
        {
          print "      double val = $STATIC_OBJECT_NAME.$OPT[$i] ();\n\n";
          print "      retval = val;\n";
        }
      elsif ($TYPE[$i] eq "float")
        {
          print "      float val = $STATIC_OBJECT_NAME.$OPT[$i] ();\n\n";
          print "      retval = val;\n";
        }
      elsif ($TYPE[$i] eq "int" or $TYPE[$i] eq "octave_idx_type")
        {
          print "      int val = $STATIC_OBJECT_NAME.$OPT[$i] ();\n\n";
          print "      retval = static_cast<double> (val);\n";
        }
      elsif ($TYPE[$i] eq "std::string")
        {
          print "      retval = $STATIC_OBJECT_NAME.$OPT[$i] ();\n";
        }
      elsif ($TYPE[$i] eq "Array<int>" or $TYPE[$i] eq "Array<octave_idx_type>")
        {
          my $elt_type;
          if ($TYPE[$i] eq "Array<int>")
            {
              $elt_type = "int";
            }
          else
            {
              $elt_type = "octave_idx_type";
            }
          print "      Array<$elt_type> val = $STATIC_OBJECT_NAME.$OPT[$i] ();\n\n";
          print "      if (val.length () == 1)
        {
          retval = static_cast<double> (val(0));
        }
      else
        {
          octave_idx_type len = val.length ();
          ColumnVector tmp (len);
          for (octave_idx_type i = 0; i < len; i++)
            tmp(i) = val(i);
          retval = tmp;
        }\n";
        }
      elsif ($TYPE[$i] eq "Array<double>")
        {
          print "      Array<double> val = $STATIC_OBJECT_NAME.$OPT[$i] ();\n\n";
          print "      if (val.length () == 1)
        {
          retval = val(0);
        }
      else
        {
          retval = ColumnVector (val);
        }\n";
        }
      elsif ($TYPE[$i] eq "Array<float>")
        {
          print "      Array<float> val = $STATIC_OBJECT_NAME.$OPT[$i] ();\n\n";
          print "      if (val.length () == 1)
        {
          retval = val(0);
        }
      else
        {
          retval = FloatColumnVector (val);
        }\n";
        }
      else
        {
          die ("unknown type $TYPE[$i]");
        }

      print "    }\n";
    }

  print qq|  else
    {
      warning ("$OPT_FCN_NAME: no match for `%s'", keyword.c_str ());
    }

  return retval;\n}\n\n|;
}

sub emit_options_function
{
  print <<"_END_EMIT_OPTIONS_FUNCTION_HDR_";
DEFUN ($OPT_FCN_NAME, args, ,
  "-*- texinfo -*-\\n\\
\@deftypefn  {Built-in Function} {} $OPT_FCN_NAME ()\\n\\
\@deftypefnx {Built-in Function} {val =} $OPT_FCN_NAME (\@var{opt})\\n\\
\@deftypefnx {Built-in Function} {} $OPT_FCN_NAME (\@var{opt}, \@var{val})\\n\\
$DOC_STRING\\n\\
\\n\\
Options include\\n\\
\\n\\
\@table \@code\\n\\
_END_EMIT_OPTIONS_FUNCTION_HDR_
# FIXME: Add extra newline above

  for (my $i = 0; $i < $OPT_NUM; $i++)
    {
      print '@item @qcode{\"', $NAME[$i], '\"}\n\\', "\n";
      print $DOC_ITEM[$i] if $DOC_ITEM[$i];
    }

  print <<"_END_EMIT_OPTIONS_FUNCTION_BODY_";
\@end table\\n\\
\@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 0)
    {
      print_$CLASS_NAME (octave_stdout);
    }
  else if (nargin == 1 || nargin == 2)
    {
      std::string keyword = args(0).string_value ();

      if (! error_state)
        {
          if (nargin == 1)
            retval = show_$CLASS_NAME (keyword);
          else
            set_$CLASS_NAME (keyword, args(1));
        }
      else
        error ("$OPT_FCN_NAME: expecting keyword as first argument");
    }
  else
    print_usage ();

  return retval;
}
_END_EMIT_OPTIONS_FUNCTION_BODY_

}

sub emit_options_debug
{
  print qq|CLASS = "$CLASS"\n|;

  for (my $i = 0; $i < $OPT_NUM; $i++)
    {
      print "\nOPTION\n";
      print qq|  NAME = "$NAME[$i]"\n|;
      print qq|  TYPE = "$TYPE[$i]"\n|;
      if ($SET_ARG_TYPE[$i])
        {
          print eval ("\"  SET_ARG_TYPE = \\\"$SET_ARG_TYPE[$i]\\\"\"") . "\n";
        }
      if ($INIT_VALUE[$i])
        {
          print qq|  INIT_VALUE = "$INIT_VALUE[$i]"\n|;
        }
      if ($INIT_BODY[$i])
        {
          print "  INIT_BODY\n";
          print &substopt ($INIT_BODY[$i]);
          print "  END_INIT_BODY\n";
        }
      if ($SET_EXPR[$i])
        {
          print qq|  SET_EXPR = "$SET_EXPR[$i]"\n|;
        }
      if ($SET_BODY[$i])
        {
          print "  SET_BODY\n";
          print &substopt ($SET_BODY[$i]);
          print "  END_SET_BODY\n";
        }
      if ($SET_CODE[$i])
        {
          print "  SET_CODE\n";
          print &substopt ($SET_CODE[$i]);
          print "  END_SET_CODE\n";
        }
      print "END_OPTION\n";
    }
}

sub substopt
{
  my ($string, $optvar, $opt, $type) = @_;

  $string =~ s/\$OPTVAR/$optvar/g;
  $string =~ s/\$OPT/$opt/g;
  $string =~ s/\$TYPE/$type/g;

  return $string;
}

sub max
{
  my $max = shift;

  foreach (@_)
    {
      $max = $_ if $max < $_;
    }

  return $max;
}

################################################################################
# Subroutine processes any command line arguments
################################################################################
sub parse_options
{
  my $result;

  $opt_help = 0;
  $opt_class_header = 0;
  $opt_handler_fcns = 0;
  $opt_debug = 0;

  $result = GetOptions ("opt-class-header" => \$opt_class_header,
                        "opt-handler-fcns" => \$opt_handler_fcns,
                        "debug" => \$opt_debug,
                        "help"  => \$opt_help);

  # give user info if options incorrect or -h(elp) given
  &usage_info if (!$result or (@ARGV != 1) or $opt_help);
  if ($opt_class_header and $opt_handler_fcns)
  {
    die "Only one of [-opt-class-header | -opt-handler-fcns ] may be specified";
  }

}

################################################################################
# Subroutine displays usage information
################################################################################
sub usage_info
{
   warn <<_END_OF_USAGE_;
//////////////////////////////////////////////////////////////////////////////
USAGE : mk-opts.pl -opt-class-header|-opt-handler-fcns [-debug] [-help] DEFN_FILE
//////////////////////////////////////////////////////////////////////////////

Automatically generate C++ code for option handling code (DASSL, DASRT, etc.)
from definition file.

See the head of mk-opts.pl for a description of the format that is parsed.
_END_OF_USAGE_

  exit(1);    # exit with error code
}

