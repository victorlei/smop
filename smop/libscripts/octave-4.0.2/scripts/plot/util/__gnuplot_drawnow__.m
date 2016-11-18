## Copyright (C) 2005-2015 John W. Eaton
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
## @deftypefn {Function File} {} __gnuplot_drawnow__ (@var{h}, @var{term}, @var{file}, @var{mono}, @var{debug_file})
## Undocumented internal function.
## @end deftypefn

## Author: jwe

function __gnuplot_drawnow__ (h, term, file, mono = false, debug_file)

  if (nargin < 1 || nargin > 5 || nargin == 2)
    print_usage ();
  endif

  if (nargin >= 3 && nargin <= 5)
    ## Produce various output formats, or redirect gnuplot stream to a
    ## debug file.
    plot_stream = [];
    fid = [];
    default_plot_stream = get (h, "__plot_stream__");
    unwind_protect
      plot_stream = __gnuplot_open_stream__ (2, h);
      gnuplot_supports_term = __gnuplot_has_terminal__ (term, plot_stream);
      if (gnuplot_supports_term)
        enhanced = gnuplot_set_term (plot_stream(1), true, h, term, file);
        __go_draw_figure__ (h, plot_stream(1), enhanced, mono);
        if (nargin == 5)
          fid = fopen (debug_file, "wb");
          enhanced = gnuplot_set_term (fid, true, h, term, file);
          __go_draw_figure__ (h, fid, enhanced, mono);
        endif
      else
        error ('__gnuplot_drawnow__: the gnuplot terminal, "%s", is not available',
               gnuplot_trim_term (term));
      endif
    unwind_protect_cleanup
      set (h, "__plot_stream__", default_plot_stream);
      if (! isempty (plot_stream))
        pclose (plot_stream(1));
        if (numel (plot_stream) > 1)
          pclose (plot_stream(2));
        endif
        if (numel (plot_stream) > 2)
          waitpid (plot_stream(3));
        endif
      endif
      if (! isempty (fid))
        fclose (fid);
      endif
    end_unwind_protect
  else  # nargin == 1
    ##  Graphics terminal for display.
    plot_stream = get (h, "__plot_stream__");
    if (isempty (plot_stream))
      plot_stream = __gnuplot_open_stream__ (2, h);
      new_stream = true;
    else
      new_stream = false;
    endif
    term = gnuplot_default_term (plot_stream);
    if (strcmp (term, "dumb"))
      ## popen2 eats stdout of gnuplot, use temporary file instead
      dumb_tmp_file = tempname ();
      enhanced = gnuplot_set_term (plot_stream(1), new_stream, h,
                                   term, dumb_tmp_file);
    else
      enhanced = gnuplot_set_term (plot_stream(1), new_stream, h, term);
    endif
    __go_draw_figure__ (h, plot_stream(1), enhanced, mono);
    fflush (plot_stream(1));
    if (strcmp (term, "dumb"))
      fid = -1;
      while (fid < 0)
        pause (0.1);
        fid = fopen (dumb_tmp_file, 'r');
      endwhile
      ## reprint the plot on screen
      [a, count] = fscanf (fid, '%c', Inf);
      fclose (fid);
      if (count > 0)
        if (a(1) == 12)
          a = a(2:end);  # avoid ^L at the beginning
        endif
        puts (a);
      endif
      unlink (dumb_tmp_file);
    endif
  endif

endfunction

function enhanced = gnuplot_set_term (plot_stream, new_stream, h, term, file)
  ## Generate the gnuplot "set terminal <term> ..." command.
  ## When "term" originates from print.m, it may include other options.
  if (nargin < 4)
    ## This supports the gnuplot graphics toolkit.
    term = gnuplot_default_term (plot_stream);
    opts_str = "";
  else
    ## Get the one word terminal id and save the remaining as options to be
    ## passed on to gnuplot.  The terminal may respect the graphics toolkit.
    [term, opts_str] = gnuplot_trim_term (term);
    term = lower (term);
    if (strcmp (term, "lua"))
      ## Replace "lua tikz" with just "tikz"
      term = "tikz";
      opts_str = strrep (opts_str, "tikz", "");
    endif
  endif

  if (strfind (opts_str, "noenhanced"))
    enhanced = false;
  else
    enhanced = gnuplot_is_enhanced_term (plot_stream, term);
  endif

  ## Set the terminal.
  if (! isempty (term))

    if (enhanced)
      enh_str = "enhanced";
    else
      enh_str = "";
    endif

    if (! isempty (h) && isfigure (h))

      ## Generate gnuplot title string for plot windows.
      if (output_to_screen (term) && ! strcmp (term, "dumb"))
        fig.numbertitle = get (h, "numbertitle");
        fig.name = strrep (get (h, "name"), '"', '\"');
        if (strcmp (get (h, "numbertitle"), "on"))
          title_str = sprintf ("Figure %d", h);
        else
          title_str = "";
        endif
        if (! isempty (fig.name) && ! isempty (title_str))
          title_str = sprintf ("%s: %s", title_str, fig.name);
        elseif (! isempty (fig.name) && isempty (title_str))
          title_str = fig.name;
        endif
        if (! isempty (title_str))
          title_str = sprintf ('title "%s"', title_str);
        endif
        if (strcmp (term, "aqua"))
          ## Adjust axes-label and tick-label spacing.
          opts_str = sprintf ('%s font "%s,%d"', opts_str,
                              get (0, "defaultaxesfontname"),
                              get (0, "defaultaxesfontsize") / 1.5);
        endif
      else
        title_str = "";
      endif

      if (! (any (strfind (opts_str, " size ") > 0)
          || any (strfind (opts_str, "size ") == 1)))
        ## Get figure size in pixels.  Rely on listener to handle coversion.
        units = get (h, "units");
        unwind_protect
          set (h, "units", "pixels");
          position_in_pixels = get (h, "position");
        unwind_protect_cleanup
          set (h, "units", units);
        end_unwind_protect
        gnuplot_pos = position_in_pixels(1:2);
        gnuplot_size = position_in_pixels(3:4);
        if (! (output_to_screen (term)
               || any (strcmp (term, {"canvas", "emf", "gif", "jpeg", ...
                                      "pbm", "png", "pngcairo", "svg"}))))
          ## Convert to inches
          gnuplot_pos = gnuplot_pos / 72;
          gnuplot_size = gnuplot_size / 72;
        endif
        if (all (gnuplot_size > 0))
          terminals_with_size = {"canvas", "emf", "epslatex", "fig", ...
                                 "gif", "jpeg", "latex", "pbm", "pdf", ...
                                 "pdfcairo", "postscript", "png", ...
                                 "pngcairo", "pstex", "pslatex", "svg", "tikz"};
          if (__gnuplot_has_feature__ ("windows_figure_position"))
            terminals_with_size{end+1} = "windows";
          endif
          if (__gnuplot_has_feature__ ("x11_figure_position"))
            terminals_with_size{end+1} = "x11";
          endif
          if (__gnuplot_has_feature__ ("wxt_figure_size"))
            terminals_with_size{end+1} = "wxt";
          endif
          switch (term)
            case terminals_with_size
              size_str = sprintf ("size %.12g,%.12g", gnuplot_size);
            case "tikz"
              size_str = sprintf ("size %gin,%gin", gnuplot_size);
            case "dumb"
              new_stream = 1;
              if (! isempty (getenv ("COLUMNS"))
                  && ! isempty (getenv ("LINES")))
                ## Let dumb use full text screen size (minus prompt lines).
                n = sprintf ("%i",
                             -2 - length (find (sprintf ("%s", PS1) == "\n")));
                ## n = the number of times \n appears in PS1
                size_str = ["size " getenv("COLUMNS") "," getenv("LINES") n];
              else
                ## Use the gnuplot default.
                size_str = "";
              endif
            case {"aqua", "fig", "corel"}
              size_str = sprintf ("size %g %g", gnuplot_size);
            case "dxf"
              size_str = "";
            otherwise
              size_str = "";
          endswitch
          if ((strcmp (term, "x11")
               && __gnuplot_has_feature__ ("x11_figure_position"))
              || (strcmpi (term, "windows")
                  && __gnuplot_has_feature__ ("windows_figure_position")))
            ## X11/Windows allows the window to be positioned as well.
            units = get (0, "units");
            unwind_protect
              set (0, "units", "pixels");
              screen_size = get (0, "screensize")(3:4);
            unwind_protect_cleanup
              set (0, "units", units);
            end_unwind_protect
            if (all (screen_size > 0))
              ## For X11/Windows, set the figure positon as well as the size
              ## gnuplot position is UL, Octave's is LL (same for screen/window)
              gnuplot_pos(2) = screen_size(2) - gnuplot_pos(2) - gnuplot_size(2);
              gnuplot_pos = max (gnuplot_pos, 1);
              size_str = sprintf ("%s position %d,%d", size_str,
                                  gnuplot_pos(1), gnuplot_pos(2));
            endif
          endif
        else
          size_str = "";
          warning ("gnuplot_set_term: size is zero");
        endif
      else
        ## A specified size take priority over the figure properies.
        size_str = "";
      endif
    else
      if (isempty (h))
        disp ("gnuplot_set_term: figure handle is empty");
      elseif (! isfigure (h))
        disp ("gnuplot_set_term: not a figure handle");
      endif
      title_str = "";
      size_str = "";
    endif

    ## Set the gnuplot terminal (type, enhanced, title, options & size).
    term_str = ["set terminal " term];
    if (__gnuplot_has_feature__ ("needs_color_with_postscript") ...
        && strcmp (term, "postscript"))
      term_str = [term_str, " color"];
    endif
    if (! isempty (enh_str))
      term_str = [term_str " " enh_str];
    endif
    if (! isempty (title_str))
      term_str = [term_str " " title_str];
    endif
    if (isempty (strfind (term, "corel")))
      if (! isempty (size_str) && new_stream)
        ## size_str comes after other options to permit specification of
        ## the canvas size for terminals cdr/corel.
        term_str = [term_str " " size_str];
      endif
      if (nargin > 3 && ischar (opts_str))
        ## Options must go last.
        term_str = [term_str " " opts_str];
      endif
    else
      if (nargin > 3 && ischar (opts_str))
        ## Options must go last.
        term_str = [term_str " " opts_str];
      endif
      if (! isempty (size_str) && new_stream)
        ## size_str comes after other options to permit specification of
        ## the canvas size for terminals cdr/corel.
        term_str = [term_str " " size_str];
      endif
    endif
    if (! __gnuplot_has_feature__ ("has_termoption_dashed"))
      ## If "set termoption dashed" isn't available add "dashed" option
      ## to the "set terminal ..." command, if it is supported.
      if (any (strcmp (term, {"aqua", "cgm", "eepic", "emf", "epslatex", ...
                              "fig", "pcl5", "mp", "next", "openstep", ...
                              "pdf", "pdfcairo", "pngcairo", "postscript", ...
                              "pslatex", "pstext", "svg", "tgif", "x11"})))
        term_str = [term_str " dashed"];
      endif
    endif
    if (any (strcmp (term, {"aqua", "wxt"})))
      term_str = [term_str, " ", "dashlength 1"];
    elseif (any (strcmp (term, {"epslatex", "postscript", "pslatex"})))
      term_str = [term_str, " ", "dashlength 2"];
    endif

    ## Work around the gnuplot feature of growing the x11 window and
    ## flickering window (x11, windows, & wxt) when the mouse and
    ## multiplot are set in gnuplot.
    fputs (plot_stream, "unset multiplot;\n");
    flickering_terms = {"x11", "windows", "wxt", "dumb"};
    if (! any (strcmp (term, flickering_terms))
        || have_non_legend_axes (h)
        || numel (findall (h, "type", "image")) > 0)
      fprintf (plot_stream, "%s\n", term_str);
      if (nargin == 5)
        if (! isempty (file))
          fprintf (plot_stream, "set output '%s';\n", file);
        endif
      endif
      fputs (plot_stream, "set multiplot;\n");
    elseif (any (strcmp (term, flickering_terms)))
      fprintf (plot_stream, "%s\n", term_str);
      if (nargin == 5)
        if (! isempty (file))
          fprintf (plot_stream, "set output '%s';\n", file);
        endif
      endif
    endif
    if (__gnuplot_has_feature__ ("has_termoption_dashed"))
      fprintf (plot_stream, "set termoption dashed\n")
    endif
  else
    ## gnuplot will pick up the GNUTERM environment variable itself
    ## so no need to set the terminal type if not also setting the
    ## figure title, enhanced mode, or position.
  endif

endfunction

function term = gnuplot_default_term (plot_stream)
  term = lower (getenv ("GNUTERM"));
  ## If not specified, guess the terminal type.
  if (isempty (term) || ! __gnuplot_has_terminal__ (term, plot_stream))
    if (isguirunning () && __gnuplot_has_terminal__ ("qt", plot_stream))
      term = "qt";
    elseif (ismac ())
      term = "aqua";
    elseif (! isunix ())
      term = "windows";
    elseif (! isempty (getenv ("DISPLAY")))
      term = "x11";
    else
      term = "dumb";
    endif
  endif
endfunction

function [term, opts] = gnuplot_trim_term (string)
  ## Extract the terminal type and terminal options (from print.m)
  string = strtrim (string);
  [term, opts] = strtok (string, ' ');
  if (! isempty (opts))
    opts(1) = "";  # trim extra space from strtok
  endif
endfunction

function have_enhanced = gnuplot_is_enhanced_term (plot_stream, term)
  ## Don't include pstex, pslatex or epslatex here as the TeX commands
  ## should not be interpreted in that case.
  persistent enhanced_terminals = {"aqua", "canvas", "dumb", "emf", "gif", ...
                                   "jpeg", "pdf", "pdfcairo", "pm", "png", ...
                                   "pngcairo", "postscript", "qt", "svg",  ...
                                   "windows", "wxt", "x11"};

  if (nargin < 2)
    ## Determine the default gnuplot terminal.
    term = gnuplot_default_term (plot_stream);
  endif
  have_enhanced = any (strcmp (term, enhanced_terminals));
endfunction

function ret = output_to_screen (term)
  ret = any (strcmpi (term,
                     {"aqua", "dumb", "pm", "qt", "windows", "wxt", "x11"}));
endfunction

function retval = have_non_legend_axes (h)
  retval = false;
  all_axes = findall (h, "type", "axes");
  if (! isempty (all_axes))
    n_all_axes = numel (all_axes);
    all_axes_tags = get (all_axes, "tag");
    legend_axes = strcmp (all_axes_tags, "legend");
    if (! isempty (legend_axes))
      n_legend_axes = sum (legend_axes);
      retval = (n_all_axes - n_legend_axes) > 1;
    endif
  endif
endfunction


## No test needed for internal helper function.
%!assert (1)

