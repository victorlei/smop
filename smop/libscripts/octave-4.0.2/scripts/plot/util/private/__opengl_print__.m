## Copyright (C) 2010-2015 Shai Ayal
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
## @deftypefn {Function File} {} __opengl_print__ (@var{@dots{}})
## Undocumented internal function.
## @end deftypefn

function opts = __opengl_print__ (opts)

  dos_shell = (ispc () && ! isunix ());

  set (0, "currentfigure", opts.figure);
  drawnow ("expose");

  if (! isempty (opts.fig2dev_binary))
    ## fig2dev is prefered for conversion to emf
    fig2dev_devices = {"pstex", "mf", "emf"};
  else
    fig2dev_devices = {"pstex", "mf"};
  endif

  gl2ps_device = {};
  pipeline = {};
  switch (lower (opts.devopt))
    case {"eps", "eps2", "epsc", "epsc2"}
      ## format GL2PS_EPS
      gl2ps_device = {"eps"};
      ## FIXME: use epstool to tighten bbox and provide preview.
      pipeline = {opts.epstool_cmd(opts, "-", opts.name)};
    case {"epslatex", "pslatex", "pdflatex", "epslatexstandalone", ...
          "pslatexstandalone", "pdflatexstandalone"}
      ## format GL2PS_TEX
      n = find (opts.devopt == "l", 1);
      suffix = opts.devopt(1:n-1);
      dot = find (opts.name == ".", 1, "last");
      if ((! isempty (dot))
          && any (strcmpi (opts.name(dot:end), ...
                  {strcat(".", suffix), ".tex", "."})))
        name = opts.name(1:dot-1);
        if (dot < numel (opts.name)
            && any (strcmpi (opts.name(dot+1:end), {"eps", "ps", "pdf"})))
          ## If user provides eps/ps/pdf suffix, use it.
          suffix = opts.name(dot+1:end);
        endif
      else
        error ("print:invalid-suffix",
               "invalid suffix '%s' for device '%s'.",
               opts.name(dot:end), lower (opts.devopt));
      endif
      gl2ps_device = {sprintf("%snotxt", lower (suffix))};
      gl2ps_device{2} = "tex";
      if (dos_shell)
        ## FIXME: this will only work on MinGW with the MSYS shell
        pipeline = {sprintf("cat > %s-inc.%s", name, suffix)};
        pipeline{2} = sprintf ("cat > %s.tex", name);
      else
        pipeline = {sprintf("cat > %s-inc.%s", name, suffix)};
        pipeline{2} = sprintf ("cat > %s.tex", name);
      endif
    case "tikz"
      ## format GL2PS_PGF
      gl2ps_device = {"pgf"};
      pipeline = {sprintf("cat > %s", opts.name)};
    case "svg"
      ## format GL2PS_SVG
      gl2ps_device = {"svg"};
      pipeline = {sprintf("cat > %s", opts.name)};
    case fig2dev_devices
      cmd_pstoedit = opts.pstoedit_cmd (opts, "fig");
      cmd_fig2dev = opts.fig2dev_cmd (opts, opts.devopt);
      if (strcmp (opts.devopt, "pstex"))
        [~, ~, ext] = fileparts (opts.name);
        if (any (strcmpi (ext, {".ps", ".tex", "."})))
          opts.name = opts.name(1:end-numel(ext));
        endif
        opts.name = strcat (opts.name, ".ps");
        cmd = sprintf ("%s | %s > %s", cmd_pstoedit, cmd_fig2dev, opts.name);
        gl2ps_device = {"eps"};
        pipeline = {cmd};
        cmd_fig2dev = opts.fig2dev_cmd (opts, "pstex_t");
        gl2ps_device{2} = "eps";
        pipeline{2} = sprintf ("%s | %s > %s", cmd_pstoedit,
                               cmd_fig2dev, strrep(opts.name, ".ps", ".tex"));
      else
        cmd = sprintf ("%s | %s > %s", cmd_pstoedit, cmd_fig2dev, opts.name);
        gl2ps_device = {"eps"};
        pipeline = {cmd};
      endif
    case "aifm"
      cmd = opts.pstoedit_cmd (opts, "ps2ai");
      gl2ps_device = {"eps"};
      pipeline = {sprintf("%s > %s", cmd, opts.name)};
    case {"dxf", "emf", "fig", "hpgl"}
      cmd = opts.pstoedit_cmd (opts);
      gl2ps_device = {"eps"};
      pipeline = {sprintf("%s > %s", cmd, opts.name)};
    case {"corel", "gif"}
      error ("print:unsupporteddevice",
             "print.m: %s output is not available for the FLTK graphics toolkit",
             upper (opts.devopt));
    case opts.ghostscript.device
      opts.ghostscript.source = "-";
      opts.ghostscript.output = opts.name;
      if (opts.send_to_printer)
        opts.unlink(strcmp (opts.unlink, opts.ghostscript.output)) = [];
        opts.ghostscript.output = "-";
      endif
      [cmd_gs, cmd_cleanup] = __ghostscript__ (opts.ghostscript);
      if (opts.send_to_printer || isempty (opts.name))
        cmd_lpr = opts.lpr_cmd (opts);
        cmd = sprintf ("%s | %s", cmd_gs, cmd_lpr);
      else
        cmd = sprintf ("%s", cmd_gs);
      endif
      if (! isempty (cmd_cleanup))
        gl2ps_device = {"eps"};
        if (dos_shell)
          pipeline = {sprintf("%s & %s", cmd, cmd_cleanup)};
        else
          pipeline = {sprintf("%s ; %s", cmd, cmd_cleanup)};
        endif
      else
        gl2ps_device = {"eps"};
        pipeline = {cmd};
      endif
    otherwise
      error (sprintf ("print:no%soutput", opts.devopt),
             "print.m: %s output is not available for GL2PS output",
             upper (opts.devopt));
  endswitch

  opts.pipeline = pipeline;

  ## Tell gl2ps to use different rendering options for 2D plots
  haxes = findall (opts.figure, "type", "axes");
  vw = get (haxes, "view");
  if (iscell (vw))
    vw = vertcat (vw{:});
  endif
  is2D = all (abs (vw(:,2)) == 90);
  if (is2D)
    gl2ps_device{end} = [gl2ps_device{end}, "is2D"];
  endif

  for n = 1:numel (pipeline)
    if (opts.debug)
      fprintf ("opengl-pipeline: '%s'\n", pipeline{n});
    endif

    if (strcmp (get (opts.figure, "visible"), "on"))
      ## Use toolkits "print_figure" method
      drawnow (gl2ps_device{n}, ['|' pipeline{n}]);
    else
      ## Use OpenGL offscreen rendering with OSMesa
      __osmesa_print__ (opts.figure, ['|' pipeline{n}], gl2ps_device{n});
    endif
  endfor

  if (! isempty (strfind (opts.devopt, "standalone")))
    opts.latex_standalone (opts);
  endif

endfunction

