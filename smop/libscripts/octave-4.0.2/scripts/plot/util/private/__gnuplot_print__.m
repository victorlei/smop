## Copyright (C) 1999-2015 Daniel Heiserer
## Copyright (C) 2001 Laurent Mazet
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
## @deftypefn {Function File} {} __gnuplot_print__ (@var{@dots{}})
## Undocumented internal function.
## @end deftypefn

## Author: Daniel Heiserer <Daniel.heiserer@physik.tu-muenchen.de>
## Adapted-By: jwe

function opts = __gnuplot_print__ (opts)

  dos_shell = (ispc () && ! isunix ());

  if (isempty (opts.fontsize))
    ## If no fontsize, determine the nominal axes fontsize.
    defaultfontsize = get (0, "defaultaxesfontsize");
    axesfontsize = get (findobj (opts.figure, "type", "axes"), "fontsize");
    if (iscell (axesfontsize))
      axesfontsize = round (median (cell2mat (axesfontsize)));
    endif
    if (isempty (axesfontsize))
      opts.fontsize = defaultfontsize;
    else
      opts.fontsize = axesfontsize;
    endif
  endif
  ## The axes-label and tick-label spacing is determined by
  ## the font spec given in "set terminal ..."
  gp_opts = font_spec (opts);

  pipeline = "";

  switch (lower (opts.devopt))
    case {"eps", "eps2", "epsc", "epsc2"}
      if (any (strcmp (opts.devopt, {"eps", "epsc"})))
        gp_opts = [gp_opts " level1"];
      endif
      if (opts.tight_flag || ! isempty (opts.preview))
        tmp_file = [tempname() ".eps"];
        eps_drawnow (opts, tmp_file, gp_opts);
        if (dos_shell)
          cleanup = [" & del " strrep(tmp_file, '/', '\')];
        else
          cleanup = [" ; rm " tmp_file];
        endif
        pipeline = {sprintf("%s %s",
                            opts.epstool_cmd (opts, tmp_file, opts.name),
                            cleanup)};
      else
        eps_drawnow (opts, opts.name, gp_opts);
      endif
    case {"epslatex", "pslatex", "pstex", "epslatexstandalone"}
      dot = find (opts.name == ".", 1, "last");
      n = find (opts.devopt == "l", 1);
      suffix = opts.devopt(1:n-1);
      if (! isempty (dot))
        if (any (strcmpi (opts.name(dot:end), {["." suffix], ".tex", "."})))
          name = opts.name(1:dot-1);
        else
          error ("print:invalid-suffix",
                 "invalid suffix '%s' for device '%s'.",
                 opts.name(dot:end), lower (opts.devopt));
        endif
      endif
      if (strfind (opts.devopt, "standalone"))
        term = sprintf ("%s ",
                        strrep (opts.devopt, "standalone", " standalone"));
      else
        term = sprintf ("%s ", opts.devopt);
      endif
      if (__gnuplot_has_feature__ ("epslatex_implies_eps_filesuffix"))
        suffix = "tex";
      else
        ## Gnuplot 4.0 wants a ".eps" suffix.
        suffix = "eps";
      endif
      local_drawnow ([term " " gp_opts],
                     strcat (name, ".", suffix), opts);
    case "tikz"
      if (__gnuplot_has_terminal__ ("tikz"))
        local_drawnow (["lua tikz " gp_opts], opts.name, opts);
      else
        error (sprintf ("print:no%soutput", opts.devopt),
               "print.m: '%s' output is not available for gnuplot-%s",
               upper (opts.devopt), __gnuplot_version__ ());
      endif
    case "svg"
      local_drawnow (["svg dynamic " gp_opts], opts.name, opts);
    case {"aifm", "corel", "eepic", "emf", "fig"}
      local_drawnow ([opts.devopt " " gp_opts], opts.name, opts);
    case {"pdfcairo", "pngcairo"}
      if (__gnuplot_has_terminal__ (opts.devopt))
        local_drawnow ([opts.devopt " " gp_opts], opts.name, opts);
      else
        error (sprintf ("print:no%soutput", opts.devopt),
               "print.m: '%s' output is not available for gnuplot-%s",
               upper (opts.devopt), __gnuplot_version__ ());
      endif
    case {"canvas", "dxf", "hpgl", "mf", "gif", "pstricks", "texdraw"}
      local_drawnow ([opts.devopt " " gp_opts], opts.name, opts);
    case opts.ghostscript.device
      gp_opts = font_spec (opts, "devopt", "eps");
      opts.ghostscript.output = opts.name;
      opts.ghostscript.source = [tempname() ".eps"];
      eps_drawnow (opts, opts.ghostscript.source, gp_opts);
      [cmd_gs, cmd_cleanup] = __ghostscript__ (opts.ghostscript);
      if (opts.send_to_printer || isempty (opts.name))
        cmd_lpr = opts.lpr_cmd (opts);
        cmd = [cmd_gs " | " cmd_lpr];
      else
        cmd = cmd_gs;
      endif
      if (dos_shell)
        cmd = sprintf ("%s & del %s", cmd,
                       strrep (opts.ghostscript.source, '/', '\'));
      else
        cmd = sprintf ("%s ; rm %s", cmd, opts.ghostscript.source);
      endif
      if (! isempty (cmd_cleanup))
        if (dos_shell)
          pipeline = {[cmd " & " cmd_cleanup]};
        else
          pipeline = {[cmd " ; " cmd_cleanup]};
        endif
      else
        pipeline = {cmd};
      endif
    otherwise
      error (sprintf ("print:no%soutput", opts.devopt),
             "print.m: %s output is not available for the Gnuplot graphics toolkit",
             upper (opts.devopt));
  endswitch


  opts.pipeline = pipeline;

  for n = 1:numel (pipeline)
    if (opts.debug)
      fprintf ("gnuplot-pipeline: '%s'\n", pipeline{n});
    endif
    [status, output] = system (pipeline{n});
    if (status)
      fprintf ("%s\n%s\n%s\n",
               "---------- output begin ----------",
               output,
               "----------- output end -----------");
      error ("gnuplot:failedpipe", "print: failed to print");
    endif
  endfor

endfunction

function eps_drawnow (opts, epsfile, gp_opts)
  [h, fontsize] = get_figure_text_objs (opts);
  unwind_protect
    fontsize_2x = cellfun (@times, {2}, fontsize, "uniformoutput", false);
    set (h, {"fontsize"}, fontsize_2x);
    local_drawnow (["postscript eps " gp_opts], epsfile, opts);
  unwind_protect_cleanup
    set (h, {"fontsize"}, fontsize);
  end_unwind_protect
endfunction

function local_drawnow (term, file, opts)
  if (opts.use_color < 0)
    mono = true;
  else
    mono = false;
  endif
  set (0, "currentfigure", opts.figure);
  if (isempty (opts.debug_file) || ! opts.debug)
    drawnow (term, file, mono);
  else
    drawnow (term, file, mono, opts.debug_file);
  endif
endfunction

function f = font_spec (opts, varargin)
  for n = 1:2:numel (varargin)
    opts.(varargin{n}) = varargin{n+1};
  endfor
  f = "";
  switch (opts.devopt)
    case "cgm"
      if (! isempty (opts.font) && ! isempty (opts.fontsize))
        f = sprintf ('font "%s,%d"', opts.font, opts.fontsize);
      elseif (! isempty (opts.font))
        f = sprintf ('font "%s"', opts.font);
      elseif (! isempty (opts.fontsize))
        f = sprintf ("%d", opts.fontsize);
      endif
    case {"eps", "eps2", "epsc", "epsc2"}
      ## Gnuplot renders fonts as half their specification, which
      ## results in a tight spacing for the axes-labels and tick-labels.
      ## Compensate for the half scale. This will produce the proper
      ## spacing for the requested fontsize.
      if (! isempty (opts.font) && ! isempty (opts.fontsize))
        f = sprintf ('font "%s,%d"', opts.font, 2 * opts.fontsize);
      elseif (! isempty (opts.font))
        f = sprintf ('font "%s"', opts.font);
      elseif (! isempty (opts.fontsize))
        f = sprintf ("%d", 2 * opts.fontsize);
      endif
    case "svg"
      ## FIXME: Why does svg format use round on the fontsize while
      ##        other terminals don't?
      if (! isempty (opts.font) && ! isempty (opts.fontsize))
        fontsize = round (opts.fontsize * 0.75);
        f = sprintf ('fname "%s" fsize %d', opts.font, fontsize);
      elseif (! isempty (opts.font))
        f = sprintf ('fname "%s"', opts.font);
      elseif (! isempty (opts.fontsize))
        fontsize = round (opts.fontsize * 0.75);
        f = sprintf ("%s fsize %d", f, fontsize);
      endif
    case "pdf"
      if (! isempty (opts.font) && ! isempty (opts.fontsize))
        f = sprintf ('font "%s,%d"', opts.font, opts.fontsize);
      elseif (! isempty (opts.font))
        f = sprintf ('font "%s"', opts.font);
      elseif (! isempty (opts.fontsize))
        f = sprintf ("fsize %d", f, opts.fontsize);
      endif
    case {"pdfcairo", "pngcairo"}
      if (! isempty (opts.font))
        f = sprintf ('font "%s"', opts.font);
      endif
    case {"epslatex", "epslatexstandalone"}
      if (! isempty (opts.font) && ! isempty (opts.fontsize))
        f = sprintf ('font "%s,%d"', opts.font, opts.fontsize);
      elseif (! isempty (opts.font))
        f = sprintf ('font "%s"', opts.font);
      elseif (! isempty (opts.fontsize))
        f = sprintf ("%d", opts.fontsize);
      endif
    case "pslatex"
      if (! isempty (opts.fontsize))
        f = sprintf ("%d", opts.fontsize);
      endif
    case {"gif", "jpeg", "png"}
      if (! isempty (opts.font) && ! isempty (opts.fontsize))
        f = sprintf ('font "%s ,%d"', opts.font, opts.fontsize);
      elseif (! isempty (opts.font))
        f = sprintf ('font "%s"', opts.font);
      elseif (! isempty (opts.fontsize))
        f = sprintf ('font "%d"', opts.fontsize);
      endif
    case "emf"
      if (! isempty (opts.font) && ! isempty (opts.fontsize))
        f = sprintf ('"%s" %d', opts.font, opts.fontsize);
      elseif (! isempty (opts.font))
        f = sprintf ('"%s"', opts.font);
      elseif (! isempty (opts.fontsize))
        f = sprintf ("%d", opts.fontsize);
      endif
    case "canvas"
      if (! isempty (opts.fontsize))
        f = sprintf ("fsize %d", opts.fontsize);
      endif
    case {"aifm", "corel"}
      if (! isempty (opts.font) && ! isempty (opts.fontsize))
        f = sprintf ("%s %d", opts.font, opts.fontsize);
      elseif (! isempty (opts.font))
        f = sprintf ("%s", opts.font);
      elseif (! isempty (opts.fontsize))
        f = sprintf ("%d", opts.fontsize);
      endif
    case "fig"
      if (! isempty (opts.font) && ! isempty (opts.fontsize))
        f = sprintf ("font %s fontsize %d", opts.font, opts.fontsize);
      elseif (! isempty (opts.font))
        f = sprintf ("font %s", opts.font);
      elseif (! isempty (opts.fontsize))
        f = sprintf ("fontsize %d", opts.fontsize);
      endif
  endswitch
endfunction

function [h, fontsize] = get_figure_text_objs (opts)
  h = findall (opts.figure, "-property", "fontsize");
  hp = get (h, "parent");
  if (iscell (hp))
    hp = cell2mat (hp);
  endif
  ## Do not change the text objects fontsizes for the children of a
  ## legend axes.  These will be handled by the fontsize listener.
  is_legend_key_string = strcmp (get (hp, "tag"), "legend") ...
                       & isprop (hp, "string") ...
                       & isprop (hp, "location") ...
                       & strcmp (get (hp, "type"), "axes");
  h(is_legend_key_string) = [];
  fontsize = get (h, "fontsize");
  switch (numel (fontsize))
    case 0
      fontsize = {};
    case 1
      fontsize = {fontsize};
  endswitch
endfunction

