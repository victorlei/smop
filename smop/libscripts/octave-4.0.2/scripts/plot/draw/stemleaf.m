## Copyright (C) 2013-2015 Michael D. Godfrey
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or
## modify it under the terms of the GNU General Public
## License as published by the Free Software Foundation;
## either version 3 of the License, or (at your option) any
## later version.
##
## Octave is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied
## warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
## PURPOSE. See the GNU General Public License for more
## details.
##
## You should have received a copy of the GNU General Public
## License along with Octave; see the file COPYING. If not,
## see <http://www.gnu.org/licenses/>.


## -*- texinfo -*-
## @deftypefn  {Function File} {} stemleaf (@var{x}, @var{caption})
## @deftypefnx {Function File} {} stemleaf (@var{x}, @var{caption}, @var{stem_sz})
## @deftypefnx {Function File} {@var{plotstr} =} stemleaf (@dots{})
## Compute and display a stem and leaf plot of the vector @var{x}.
##
## The input @var{x} should be a vector of integers.  Any non-integer values
## will be converted to integer by @code{@var{x} = fix (@var{x})}.  By default
## each element of @var{x} will be plotted with the last digit of the element
## as a leaf value and the remaining digits as the stem.  For example, 123
## will be plotted with the stem @samp{12} and the leaf @samp{3}.  The second
## argument, @var{caption}, should be a character array which provides a
## description of the data.  It is included as a heading for the output.
##
## The optional input @var{stem_sz} sets the width of each stem.
## The stem width is determined by @code{10^(@var{stem_sz} + 1)}.
## The default stem width is 10.
##
## The output of @code{stemleaf} is composed of two parts: a
## "Fenced Letter Display," followed by the stem-and-leaf plot itself.
## The Fenced Letter Display is described in @cite{Exploratory Data Analysis}.
## Briefly, the entries are as shown:
##
## @example
## @group
##
##         Fenced Letter Display
## #% nx|___________________     nx = numel (x)
## M% mi|       md         |     mi median index, md median
## H% hi|hl              hu| hs  hi lower hinge index, hl,hu hinges,
## 1    |x(1)         x(nx)|     hs h_spreadx(1), x(nx) first
##            _______            and last data value.
##      ______|step |_______     step 1.5*h_spread
##     f|ifl            ifh|     inner fence, lower and higher
##      |nfl            nfh|     no.\ of data points within fences
##     F|ofl            ofh|     outer fence, lower and higher
##      |nFl            nFh|     no.\ of data points outside outer
##                               fences
## @end group
## @end example
##
## The stem-and-leaf plot shows on each line the stem value followed by the
## string made up of the leaf digits.  If the @var{stem_sz} is not 1 the
## successive leaf values are separated by ",".
##
## With no return argument, the plot is immediately displayed.  If an output
## argument is provided, the plot is returned as an array of strings.
##
## The leaf digits are not sorted.  If sorted leaf values are desired, use
## @code{@var{xs} = sort (@var{x})} before calling @code{stemleaf (@var{xs})}.
##
## The stem and leaf plot and associated displays are described in:
## Ch. 3, @cite{Exploratory Data Analysis} by J. W. Tukey, Addison-Wesley, 1977.
## @seealso{hist, printd}
## @end deftypefn

## Author: Michael D. Godfrey <michaeldgodfrey@gmail.com>
## Description: Compute stem and leaf plot

function plotstr = stemleaf (x, caption, stem_sz)
  ## Compute and display a stem and leaf plot of the vector x.  The x
  ## vector is converted to integer by x = fix(x).  If an output argument
  ## is provided, the plot is returned as an array of strings.  The
  ## first element is the heading followed by an element for each stem.
  ##
  ## The default stem step is 10.  If stem_sz is provided the stem
  ## step is set to: 10^(stem_sz+1).  The x vector should be integers.
  ## It will be treated so that the last digit is the leaf value and the
  ## other digits are the stems.
  ##
  ## When we first implemented stem and leaf plots in the early 1960's
  ## there was some discussion about sorting vs. leaving the leaf
  ## entries in the original order in the data.  We decided in favor of
  ## sorting the leaves for most purposes.  This is the choice
  ## implemented in the SNAP/IEDA system that was written at that time.
  ##
  ## SNAP/IEDA, and particularly its stem and leaf plotting, were further
  ## developed by Hale Trotter, David Hoagland (at Princeton and MIT),
  ## and others.
  ##
  ## Tukey, in EDA, generally uses unsorted leaves.  In addition, he
  ## described a wide range of additional display formats.  This
  ## implementation does not sort the leaves, but if the x vector is
  ## sorted then the leaves come out sorted.  A simple display format is
  ## used.
  ##
  ## I doubt if providing other options is worthwhile.  The code can
  ## quite easily be modified to provide specific display results.  Or,
  ## the returned output string can be edited.  The returned output is an
  ## array of strings with each row containing a line of the plot
  ## preceded by the lines of header text as the first row.  This
  ## facilitates annotation.
  ##
  ## Note that the code has some added complexity due to the need to
  ## distinguish both + and - 0 stems.  The +- stem values are essential
  ## for all plots which span 0. After dealing with +-0 stems, the added
  ## complexity of putting +- data values in the correct stem is minor,
  ## but the sign of 0 leaves must be checked.  And, the cases where the
  ## stems start or end at +- 0 must also be considered.
  ##
  ## The fact that IEEE floating point defines +- 0 helps make this
  ## easier.
  ##
  ## Michael D. Godfrey   January 2013

  ## More could be implemented for better data scaling.  And, of course,
  ## other options for the kinds of plots described by Tukey could be
  ## provided.  This may best be left to users.

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! isvector (x))
    error ("stemleaf: X must be a vector");
  endif

  if (isinteger (x))
    ## Avoid use of integers because rounding rules do not use fix():
    ## Example: floor (int32 (-44)/10) == -4, floor (int32 (-46)/10) = -5 !!!
    x = single (x);
  elseif (isfloat (x))
    xint = fix (x);
    if (any (x != xint))
      warning ("stemleaf: X truncated to integer values");
      x = xint;
    endif
  else
    error ("stemleaf: X must be a numeric vector");
  endif

  if (! ischar (caption))
    error ("stemleaf: CAPTION must be a character array");
  endif

  if (nargin == 2)
    stem_step = 10;
  else
    if (isscalar (stem_sz) && stem_sz >= 0 && isreal (stem_sz))
      stem_sz = fix (stem_sz);
      stem_step = 10^(stem_sz+1);
    else
      error ("stemleaf: STEM_SZ must be a real integer >= 0");
    endif
  endif

  ## Note that IEEE 754 states that -+ 0 should compare equal. This has
  ## led to C sort (and therefore Octave) treating them as equal.  Thus,
  ## sort([-1 0 -0 1]) yields [-1 0 -0 1], and sort([-1 -0 0 1])
  ## yields: [-1 -0 0 1].  This means that stem-and-leaf plotting cannot
  ## rely on sort to order the data as needed for display.
  ## This also applies to min()/max() so these routines can't be relied
  ## upon if the max or min is -+ 0.

  ## Compute hinges and fences based on ref: EDA pgs. 33 and 44.
  ## Note that these outlier estimates are meant to be "distribution free".

  nx = numel (x);
  xs = sort (x);                # Note that sort preserves -0
  mdidx = fix ((nx + 1)/2);     # median index
  hlidx = fix ((mdidx + 1)/2);  # lower hinge index
  huidx = fix (nx + 1 - hlidx); # upper hinge index
  md = xs(mdidx);               # median
  hl = xs(hlidx);               # lower hinge
  hu = xs(huidx);               # upper hinge
  h_spread = hu - hl;           # h_spread: difference between hinges
  step = fix (1.5*h_spread);    # step: 1.5 * h_spread
  i_fence_l = hl - step;        # inner fences: outside hinges + step
  o_fence_l = hl - 2*step;      # outer fences: outside hinges + 2*step
  i_fence_h = hu + step;
  o_fence_h = hu + 2*step;
  n_out_l   = sum (x<i_fence_l) - sum (x<o_fence_l);
  n_out_h   = sum (x>i_fence_h) - sum (x>o_fence_h);
  n_far_l   = sum (x<o_fence_l);
  n_far_h   = sum (x>o_fence_h);

  ## display table similar to that on pg. 33
  plot_out = sprintf ("       Data: %s", caption);
  plot_out = [plot_out; sprintf(" ")];
  plot_out = [plot_out; sprintf("         Fenced Letter Display")];
  plot_out = [plot_out; sprintf(" ")];
  plot_out = [plot_out; sprintf("     #%3d|___________________", nx)];
  plot_out = [plot_out; sprintf("     M%3d|       %5d      |", mdidx, md)];
  plot_out = [plot_out; sprintf("     H%3d|%5d        %5d|   %d", hlidx, hl, hu, h_spread)];
  plot_out = [plot_out; sprintf("     1   |%5d        %5d|", xs(1), xs(nx))];
  plot_out = [plot_out; sprintf("               _______")];
  plot_out = [plot_out; sprintf("         ______|%5d|_______",step)];
  plot_out = [plot_out; sprintf("        f|%5d        %5d|", i_fence_l, i_fence_h)];
  plot_out = [plot_out; sprintf("         |%5d        %5d|  out", n_out_l, n_out_h)];
  plot_out = [plot_out; sprintf("        F|%5d        %5g|", o_fence_l, o_fence_h)];
  plot_out = [plot_out; sprintf("         |%5d        %5d|  far",n_far_l,n_far_h)];
  plot_out = [plot_out; " "];

  ## Determine stem values
  min_x = min (x);
  max_x = max (x);
  if (min_x > 0)      # all stems > 0
    stems = [fix(min(x)/stem_step) : (fix(max(x)/stem_step)+1)];
  elseif (max_x < 0)  # all stems < 0
    stems = [(fix(min_x/stem_step)-1) : fix(max_x/stem_step)];
  elseif (min_x < 0 && max_x > 0)  # range crosses 0
    stems = [(fix(min_x/stem_step)-1) : -0, 0 : fix(max_x/stem_step)+1 ];
  else   # one endpoint is a zero which may be +0 or -0
    if (min_x == 0)
      if (any (x == 0 & signbit (x)))
        min_x = -0;
      else
        min_x = +0;
      endif
    endif
    if (max_x == 0)
      if (any (x == 0 & ! signbit (x)))
        max_x = +0;
      else
        max_x = -0;
      endif
    endif
    stems = [];
    if (signbit (min_x))
      stems = [(fix(min_x/stem_step)-1) : -0];
    endif
    if (! signbit (max_x))
      stems = [stems, 0 : fix(max_x/stem_step)+1 ];
    endif
  endif

  ## Vectorized version provided by Rik Wehbring (rik@octave.org)
  ## Determine leaves for each stem:
  new_line  = 1;
  for kx = 2: numel (stems)

    stem_sign = signbit (stems(kx));
    if (stems(kx) <= 0)
      idx = ((x <= stems(kx)*stem_step) & (x > (stems(kx-1)*stem_step))
              & (signbit (x) == stem_sign));
      xlf = abs (x(idx) - stems(kx)*stem_step);
    else
      idx = ((x < stems(kx)*stem_step) & (x >= (stems(kx-1)*stem_step))
              & (signbit (x) == stem_sign));
      xlf = abs (x(idx) - stems(kx-1)*stem_step);
    endif
    ## Convert leaves to a string
    if (stem_step == 10)
      lf_str = sprintf ("%d", xlf);
    else
      lf_str = "";
      if (! isempty (xlf))
        lf_str = sprintf ("%d", xlf(1));
        if (numel (xlf) > 1)
          lf_str = [lf_str sprintf(",%d", xlf(2:end))];
        endif
      endif
    endif

    ## Set correct -0
    if (stems(kx) == 0 && signbit (stems(kx)))
      line = sprintf ("  -0 | %s",  lf_str);  # -0 stem.
    elseif (stems(kx) < 0)
      line = sprintf ("%4d | %s", stems(kx), lf_str);
    elseif (stems(kx) > 0)
      line = sprintf ("%4d | %s", stems(kx-1), lf_str);
    else
      line = "";
    endif

    if (! isempty (lf_str) || stems(kx) == 0 || stems(kx-1) == 0)
      plot_out = [plot_out; line];
      new_line = 1;
    else
      if (new_line == 1)
        plot_out = [plot_out; "     :"];  # just print one : if no leaves
        new_line = 0;
      endif
    endif

  endfor    # kx = 2: numel (stems)

  if (nargout == 0)
    disp (plot_out);
  else
    plotstr = plot_out;
  endif

endfunction


%!demo
%! %% Unsorted plot:
%! x = [-22 12 -28 52  39 -2 12 10 11 11 42 38 44 18 44];
%! stemleaf (x, 'Unsorted plot');

%!demo
%! %% Sorted leaves:
%! x = [-22 12 -28 52  39 -2 12 10 11 11 42 38 44 18 44];
%! y = sort (x);
%! stemleaf (y, 'Sorted leaves');

%!demo
%! %% Sorted leaves (large dataset):
%! x = [-22 12 -28 52  39 -2 12 10 11 11 42 38 44 18 44 37 113 124 37 48     ...
%!      127 36 29 31 125 139 131 115 105 132 104 123 35 113 122 42 117 119   ...
%!      58 109 23 105 63 27 44 105 99 41 128 121 116 125 32 61 37 127 29 113 ...
%!      121 58 114 126 53 114 96 25 109 7 31 141 46 -13 71 43 117 116 27 7   ...
%!      68 40 31 115 124 42 128 52 71 118 117 38 27 106 33 117 116 111 40    ...
%!      119 47 105 57 122 109 124 115 43 120 43 27 27 18 28 48 125 107 114   ...
%!      34 133 45 120 30 127 31 116 146 21 23 30 10 20 21 30 0 100 110 1 20  ...
%!      0];
%! y = sort (x);
%! stemleaf (y, 'Sorted leaves (large dataset)');

%!demo
%! %% Gaussian leaves:
%! x = fix (30 * randn (300,1));
%! stemleaf (x, 'Gaussian leaves');

%!test
%! ## test minus to plus
%! x = [-22 12 -28 52  39 -2 12 10 11 11 42 38 44 18 44 37 113 124 37 48 127   ...
%!      36 29 31 125 139 131 115 105 132 104 123 35 113 122 42 117 119 58 109  ...
%!      23 105 63 27 44 105 99 41 128 121 116 125 32 61 37 127 29 113 121 58   ...
%!      114 126 53 114 96 25 109 7 31 141 46 -13 71 43 117 116 27 7 68 40 31   ...
%!      115 124 42 128 52 71 118 117 38 27 106 33 117 116 111 40 119 47 105 57 ...
%!      122 109 124 115 43 120 43 27 27 18 28 48 125 107 114 34 133 45 120 30  ...
%!      127 31 116 146 21 23 30 10 20 21 30 0 100 110 1 20 0];
%! x = sort (x);
%! rexp = char (
%! "       Data: test minus to plus"    ,
%! " "                                  ,
%! "         Fenced Letter Display"     ,
%! " "                                  ,
%! "     #138|___________________"      ,
%! "     M 69|          52      |"      ,
%! "     H 35|   30          116|   86" ,
%! "     1   |  -28          146|"      ,
%! "               _______"             ,
%! "         ______|  129|_______"      ,
%! "        f|  -99          245|"      ,
%! "         |    0            0|  out" ,
%! "        F| -228          374|"      ,
%! "         |    0            0|  far" ,
%! " "                                  ,
%! "  -2 | 82"                          ,
%! "  -1 | 3"                           ,
%! "  -0 | 2"                           ,
%! "   0 | 00177"                       ,
%! "   1 | 00112288"                    ,
%! "   2 | 001133577777899"             ,
%! "   3 | 000111123456777889"          ,
%! "   4 | 00122233344456788"           ,
%! "   5 | 223788"                      ,
%! "   6 | 138"                         ,
%! "   7 | 11"                          ,
%! "     : "                            ,
%! "   9 | 69"                          ,
%! "  10 | 04555567999"                 ,
%! "  11 | 0133344455566667777899"      ,
%! "  12 | 0011223444555677788"         ,
%! "  13 | 1239"                        ,
%! "  14 | 16"                          );
%! r = stemleaf (x, "test minus to plus", 0);
%! assert (r, rexp);

%!test
%! ## positive values above 0
%! x = [5 22 12 28 52 39 12 11 11 42 38 44 18 44];
%! rexp = char (
%! "       Data: positive values above 0",
%! " "                                   ,
%! "         Fenced Letter Display"      ,
%! " "                                   ,
%! "     # 14|___________________"       ,
%! "     M  7|          22      |"       ,
%! "     H  4|   12           42|   30"  ,
%! "     1   |    5           52|"       ,
%! "               _______"              ,
%! "         ______|   45|_______"       ,
%! "        f|  -33           87|"       ,
%! "         |    0            0|  out"  ,
%! "        F|  -78          132|"       ,
%! "         |    0            0|  far"  ,
%! " "                                   ,
%! "   0 | 5"                            ,
%! "   1 | 22118"                        ,
%! "   2 | 28"                           ,
%! "   3 | 98"                           ,
%! "   4 | 244"                          ,
%! "   5 | 2"                            );
%! r = stemleaf (x, "positive values above 0");
%! assert (r, rexp);

%!test
%! ## negative values below 0
%! x = [5 22 12 28 52 39 12 11 11 42 38 44 18 44];
%! x = -x;
%! rexp = char (
%! "       Data: negative values below 0",
%! " "                                   ,
%! "         Fenced Letter Display"      ,
%! " "                                   ,
%! "     # 14|___________________"       ,
%! "     M  7|         -28      |"       ,
%! "     H  4|  -42          -12|   30"  ,
%! "     1   |  -52           -5|"       ,
%! "               _______"              ,
%! "         ______|   45|_______"       ,
%! "        f|  -87           33|"       ,
%! "         |    0            0|  out"  ,
%! "        F| -132           78|"       ,
%! "         |    0            0|  far"  ,
%! " "                                   ,
%! "  -5 | 2"                            ,
%! "  -4 | 244"                          ,
%! "  -3 | 98"                           ,
%! "  -2 | 28"                           ,
%! "  -1 | 22118"                        ,
%! "  -0 | 5"                            );
%! r = stemleaf (x, "negative values below 0");
%! assert (r, rexp);

%!test
%! ## positive values from 0
%! x = [22 12 28 52 39 2 12 0 11 11 42 38 44 18 44];
%! rexp = char (
%! "       Data: positive values from 0",
%! " "                                  ,
%! "         Fenced Letter Display"     ,
%! " "                                  ,
%! "     # 15|___________________"      ,
%! "     M  8|          22      |"      ,
%! "     H  4|   11           42|   31" ,
%! "     1   |    0           52|"      ,
%! "               _______"             ,
%! "         ______|   46|_______"      ,
%! "        f|  -35           88|"      ,
%! "         |    0            0|  out" ,
%! "        F|  -81          134|"      ,
%! "         |    0            0|  far" ,
%! " "                                  ,
%! "   0 | 20"                          ,
%! "   1 | 22118"                       ,
%! "   2 | 28"                          ,
%! "   3 | 98"                          ,
%! "   4 | 244"                         ,
%! "   5 | 2"                           );
%! r = stemleaf (x, "positive values from 0");
%! assert (r, rexp);

%!test
%! ## negative values from 0
%! x = [22 12 28 52 39 2 12 0 11 11 42 38 44 18 44];
%! x = -x;
%! rexp = char (
%! "       Data: negative values from 0",
%! " "                                  ,
%! "         Fenced Letter Display"     ,
%! " "                                  ,
%! "     # 15|___________________"      ,
%! "     M  8|         -22      |"      ,
%! "     H  4|  -42          -11|   31" ,
%! "     1   |  -52            0|"      ,
%! "               _______"             ,
%! "         ______|   46|_______"      ,
%! "        f|  -88           35|"      ,
%! "         |    0            0|  out" ,
%! "        F| -134           81|"      ,
%! "         |    0            0|  far" ,
%! " "                                  ,
%! "  -5 | 2"                           ,
%! "  -4 | 244"                         ,
%! "  -3 | 98"                          ,
%! "  -2 | 28"                          ,
%! "  -1 | 22118"                       ,
%! "  -0 | 20"                          );
%! r = stemleaf (x, "negative values from 0");
%! assert (r, rexp);

%!test
%! ## both +0 and -0 present
%! x = [-9 -7 -0 0 -0];
%! rexp = char (
%! "       Data: both +0 and -0 present",
%! " "                                  ,
%! "         Fenced Letter Display"     ,
%! " "                                  ,
%! "     #  5|___________________"      ,
%! "     M  3|           0      |"      ,
%! "     H  2|   -7            0|   7"  ,
%! "     1   |   -9            0|"      ,
%! "               _______"             ,
%! "         ______|   10|_______"      ,
%! "        f|  -17           10|"      ,
%! "         |    0            0|  out" ,
%! "        F|  -27           20|"      ,
%! "         |    0            0|  far" ,
%! " "                                  ,
%! "  -0 | 9700"                        ,
%! "   0 | 0"                           );
%! r = stemleaf (x, "both +0 and -0 present");
%! assert (r, rexp);

%!test
%! ## both <= 0 and -0 present
%! x = [-9 -7 0 -0];
%! rexp = char (
%! "       Data: both <= 0 and -0 present",
%! " "                                    ,
%! "         Fenced Letter Display"       ,
%! " "                                    ,
%! "     #  4|___________________"        ,
%! "     M  2|          -7      |"        ,
%! "     H  1|   -9            0|   9"    ,
%! "     1   |   -9            0|"        ,
%! "               _______"               ,
%! "         ______|   13|_______"        ,
%! "        f|  -22           13|"        ,
%! "         |    0            0|  out"   ,
%! "        F|  -35           26|"        ,
%! "         |    0            0|  far"   ,
%! " "                                    ,
%! "  -0 | 970"                           ,
%! "   0 | 0"                             );
%! r = stemleaf (x, "both <= 0 and -0 present");
%! assert (r, rexp);

%!test
%! ##   Example from EDA: Chevrolet Prices pg. 30
%! x = [150 250 688 695 795 795 895 895 895 ...
%!      1099 1166 1333 1499 1693 1699 1775 1995];
%! rexp = char (
%! "       Data: Chevrolet Prices EDA pg.30",
%! " "                                      ,
%! "         Fenced Letter Display"         ,
%! " "                                      ,
%! "     # 17|___________________"          ,
%! "     M  9|         895      |"          ,
%! "     H  5|  795         1499|   704"    ,
%! "     1   |  150         1995|"          ,
%! "               _______"                 ,
%! "         ______| 1056|_______"          ,
%! "        f| -261         2555|"          ,
%! "         |    0            0|  out"     ,
%! "        F|-1317         3611|"          ,
%! "         |    0            0|  far"     ,
%! " "                                      ,
%! "   1 | 50"                              ,
%! "   2 | 50"                              ,
%! "     :"                                 ,
%! "   6 | 88,95"                           ,
%! "   7 | 95,95"                           ,
%! "   8 | 95,95,95"                        ,
%! "     :"                                 ,
%! "  10 | 99"                              ,
%! "  11 | 66"                              ,
%! "     :"                                 ,
%! "  13 | 33"                              ,
%! "  14 | 99"                              ,
%! "     :"                                 ,
%! "  16 | 93,99"                           ,
%! "  17 | 75"                              ,
%! "     :"                                 ,
%! "  19 | 95"                              );
%! r = stemleaf (x, "Chevrolet Prices EDA pg.30", 1);
%! assert (r, rexp);

## Test input validation
%!error stemleaf ()
%!error stemleaf (1, 2, 3, 4)
%!error <X must be a vector> stemleaf (ones (2,2), "")
%!warning <X truncated to integer values> tmp = stemleaf ([0 0.5 1],"");
%!error <X must be a numeric vector> stemleaf ("Hello World", "data")
%!error <CAPTION must be a character array> stemleaf (1, 2)
%!error <STEM_SZ must be a real integer> stemleaf (1, "", ones (2,2))
%!error <STEM_SZ must be a real integer> stemleaf (1, "", -1)
%!error <STEM_SZ must be a real integer> stemleaf (1, "", 1+i)

