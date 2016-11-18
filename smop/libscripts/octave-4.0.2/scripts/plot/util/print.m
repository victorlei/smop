## Copyright (C) 2008-2015 David Bateman
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
## @deftypefn  {Function File} {} print ()
## @deftypefnx {Function File} {} print (@var{options})
## @deftypefnx {Function File} {} print (@var{filename}, @var{options})
## @deftypefnx {Function File} {} print (@var{h}, @var{filename}, @var{options})
## Print a plot, or save it to a file.
##
## Both output formatted for printing (PDF and PostScript), and many bitmapped
## and vector image formats are supported.
##
## @var{filename} defines the name of the output file.  If the file name has
## no suffix, one is inferred from the specified device and appended to the
## file name.  If no filename is specified, the output is sent to the
## printer.
##
## @var{h} specifies the handle of the figure to print.  If no handle is
## specified the current figure is used.
##
## For output to a printer, PostScript file, or PDF file, the paper size is
## specified by the figure's @code{papersize} property.  The location and
## size of the image on the page are specified by the figure's
## @code{paperposition} property.  The orientation of the page is specified
## by the figure's @code{paperorientation} property.
##
## The width and height of images are specified by the figure's
## @code{paperpositon(3:4)} property values.
##
## The @code{print} command supports many @var{options}:
##
## @table @code
## @item -f@var{h}
##   Specify the handle, @var{h}, of the figure to be printed.  The default
## is the current figure.
##
## @item -P@var{printer}
##   Set the @var{printer} name to which the plot is sent if no
## @var{filename} is specified.
##
## @item -G@var{ghostscript_command}
##   Specify the command for calling Ghostscript.  For Unix and Windows the
## defaults are @qcode{"gs"} and @qcode{"gswin32c"}, respectively.
##
## @item  -color
## @itemx -mono
##   Color or monochrome output.
##
## @item  -solid
## @itemx -dashed
##   Force all lines to be solid or dashed, respectively.
##
## @item  -portrait
## @itemx -landscape
##   Specify the orientation of the plot for printed output.
## For non-printed output the aspect ratio of the output corresponds to the
## plot area defined by the @qcode{"paperposition"} property in the
## orientation specified.  This option is equivalent to changing the figure's
## @qcode{"paperorientation"} property.
##
## @item  -TextAlphaBits=@var{n}
## @itemx -GraphicsAlphaBits=@var{n}
##   Octave is able to produce output for various printers, bitmaps, and
## vector formats by using Ghostscript.  For bitmap and printer output
## anti-aliasing is applied using Ghostscript's TextAlphaBits and
## GraphicsAlphaBits options.  The default number of bits for each is 4.
## Allowed values for @var{N} are 1, 2, or 4.
##
## @item -d@var{device}
##   The available output format is specified by the option @var{device}, and
## is one of:
##
##   @table @code
##   @item  ps
##   @itemx ps2
##   @itemx psc
##   @itemx psc2
##     PostScript (level 1 and 2, mono and color).  The FLTK graphics
## toolkit generates PostScript level 3.0.
##
##   @item  eps
##   @itemx eps2
##   @itemx epsc
##   @itemx epsc2
##     Encapsulated PostScript (level 1 and 2, mono and color).  The FLTK
## graphic toolkit generates PostScript level 3.0.
##
##   @item  pslatex
##   @itemx epslatex
##   @itemx pdflatex
##   @itemx pslatexstandalone
##   @itemx epslatexstandalone
##   @itemx pdflatexstandalone
##     Generate a @LaTeX{} file @file{@var{filename}.tex} for the text
## portions of a plot and a file @file{@var{filename}.(ps|eps|pdf)} for the
## remaining graphics.  The graphics file suffix .ps|eps|pdf is determined
## by the specified device type.  The @LaTeX{} file produced by the
## @samp{standalone} option can be processed directly by @LaTeX{}.  The file
## generated without the @samp{standalone} option is intended to be included
## from another @LaTeX{} document.  In either case, the @LaTeX{} file
## contains an @code{\includegraphics} command so that the generated graphics
## file is automatically included when the @LaTeX{} file is processed.  The
## text that is written to the @LaTeX{} file contains the strings
## @strong{exactly} as they were specified in the plot.  If any special
## characters of the @TeX{} mode interpreter were used, the file must be
## edited before @LaTeX{} processing.  Specifically, the special characters
## must be enclosed with dollar signs (@code{$ @dots{} $}), and other
## characters that are recognized by @LaTeX{} may also need editing (.e.g.,
## braces).  The @samp{pdflatex} device, and any of the @samp{standalone}
## formats, are not available with the Gnuplot toolkit.
##
##   @item tikz
##     Generate a @LaTeX{} file using PGF/TikZ@.  For the FLTK toolkit
## the result is PGF.
##
##   @item  ill
##   @itemx aifm
##     Adobe Illustrator (Obsolete for Gnuplot versions > 4.2)
##
##   @item  cdr
##   @itemx @nospell{corel}
##     CorelDraw
##
##   @item dxf
##     AutoCAD
##
##   @item  emf
##   @itemx meta
##     Microsoft Enhanced Metafile
##
##   @item fig
##     XFig.  For the Gnuplot graphics toolkit, the additional options
## @option{-textspecial} or @option{-textnormal} can be used to control whether the special flag should be set for the text in the figure.  (default is @option{-textnormal})
##
##   @item hpgl
##     HP plotter language
##
##   @item mf
##     Metafont
##
##   @item png
##     Portable network graphics
##
##   @item  jpg
##   @itemx jpeg
##     JPEG image
##
##   @item gif
##     GIF image (only available for the Gnuplot graphics toolkit)
##
##   @item pbm
##     PBMplus
##
##   @item svg
##     Scalable vector graphics
##
##   @item pdf
##     Portable document format
##   @end table
##
##   If the device is omitted, it is inferred from the file extension,
## or if there is no filename it is sent to the printer as PostScript.
##
## @item -d@var{ghostscript_device}
##   Additional devices are supported by Ghostscript.
## Some examples are;
##
##   @table @code
##   @item pdfwrite
##     Produces pdf output from eps
##
##   @item ljet2p
##     HP LaserJet @nospell{IIP}
##
##   @item pcx24b
##     24-bit color PCX file format
##
##   @item ppm
##     Portable Pixel Map file format
##   @end table
##
##   For a complete list, type @code{system ("gs -h")} to see what formats
## and devices are available.
##
##   When Ghostscript output is sent to a printer the size is determined by
## the figure's @qcode{"papersize"} property.  When the output is sent to a
## file the size is determined by the plot box defined by the figure's
## @qcode{"paperposition"} property.
##
## @item -append
##   Append PostScript or PDF output to a pre-existing file of the same type.
##
## @item -r@var{NUM}
##   Resolution of bitmaps in pixels per inch.  For both metafiles and SVG
## the default is the screen resolution; for other formats it is 150 dpi.  To
## specify screen resolution, use @qcode{"-r0"}.
##
## @item  -loose
## @itemx -tight
##   Force a tight or loose bounding box for eps files.  The default is loose.
##
## @item -@var{preview}
##   Add a preview to eps files.  Supported formats are:
##
##   @table @code
##   @item -interchange
##     Provide an interchange preview.
##
##   @item -metafile
##     Provide a metafile preview.
##
##   @item -pict
##     Provide pict preview.
##
##   @item -tiff
##     Provide a tiff preview.
##   @end table
##
## @item -S@var{xsize},@var{ysize}
##   Plot size in pixels for EMF, GIF, JPEG, PBM, PNG, and SVG@.
## For PS, EPS, PDF, and other vector formats the plot size is in points.
## This option is equivalent to changing the size of the plot box associated
## with the @qcode{"paperposition"} property.  When using the command form of
## the print function you must quote the @var{xsize},@var{ysize} option.  For
## example, by writing @w{"-S640,480"}.
##
## @item  -F@var{fontname}
## @itemx -F@var{fontname}:@var{size}
## @itemx -F:@var{size}
##   Use @var{fontname} and/or @var{fontsize} for all text.
## @var{fontname} is ignored for some devices: dxf, fig, hpgl, etc.
## @end table
##
## The filename and options can be given in any order.
##
## Example: Print to a file using the pdf device.
##
## @example
## @group
## figure (1);
## clf ();
## surf (peaks);
## print figure1.pdf
## @end group
## @end example
##
## Example: Print to a file using jpg device.
##
## @example
## @group
## clf ();
## surf (peaks);
## print -djpg figure2.jpg
## @end group
## @end example
##
## Example: Print to printer named PS_printer using ps format.
##
## @example
## @group
## clf ();
## surf (peaks);
## print -dpswrite -PPS_printer
## @end group
## @end example
##
## @seealso{saveas, hgsave, orient, figure}
## @end deftypefn

function print (varargin)

  opts = __print_parse_opts__ (varargin{:});

  opts.pstoedit_cmd = @pstoedit;
  opts.fig2dev_cmd = @fig2dev;
  opts.latex_standalone = @latex_standalone;
  opts.lpr_cmd = @lpr;
  opts.epstool_cmd = @epstool;

  if (isempty (opts.figure) || ! isfigure (opts.figure))
    error ("print: no figure to print");
  endif

  if (isempty (findall (opts.figure, "-depth", "1", "type", "axes")))
    error ("print: no axes object in figure to print");
  endif

  orig_figure = get (0, "currentfigure");
  set (0, "currentfigure", opts.figure);

  if (opts.append_to_file)
    [~, ~, ext] = fileparts (opts.ghostscript.output);
    opts.ghostscript.prepend = [tempname() ext];
    copyfile (opts.ghostscript.output, opts.ghostscript.prepend);
  endif

  unwind_protect

    ## Modify properties as specified by options
    props = [];

    drawnow ();

    ## print() requires figure units to be "pixels"
    props(1).h = opts.figure;
    props(1).name = "units";
    props(1).value = {get(opts.figure, "units")};
    set (opts.figure, "units", "pixels");

    ## graphics toolkit translates figure position to eps bbox (points)
    fpos = get (opts.figure, "position");
    props(2).h = opts.figure;
    props(2).name = "position";
    props(2).value = {fpos};
    fpos(3:4) = opts.canvas_size;
    set (opts.figure, "position", fpos);

    ## Set figure background to none.
    ## This is done both for consistency with Matlab and to eliminate
    ## the visible box along the figure's perimeter.
    props(3).h = opts.figure;
    props(3).name = "color";
    props(3).value{1} = get (props(3).h, props(3).name);
    set (props(3).h, "color", "none");

    if (opts.force_solid != 0)
      h = findall (opts.figure, "-property", "linestyle");
      m = numel (props);
      for n = 1:numel (h)
        props(m+n).h = h(n);
        props(m+n).name = "linestyle";
        props(m+n).value = {get(h(n), "linestyle")};
      endfor
      if (opts.force_solid > 0)
        linestyle = "-";
      else
        linestyle = "--";
      endif
      set (h, "linestyle", linestyle);
    endif

    if (opts.use_color < 0
        && ! strcmp (get (opts.figure, "__graphics_toolkit__"), "gnuplot"))
      color_props = {"color", "facecolor", "edgecolor", "colormap"};
      for c = 1:numel (color_props)
        h = findall (opts.figure, "-property", color_props{c});
        hnone = findall (opts.figure, color_props{c}, "none");
        h = setdiff (h, hnone);
        m = numel (props);
        for n = 1:numel (h)
          if (ishandle (h(n)))
            ## Need to verify objects exist since callbacks may delete objects
            ## as the colors for others are modified.
            rgb = get (h(n), color_props{c});
            props(end+1).h = h(n);
            props(end).name = color_props{c};
            props(end).value = {get(h(n), color_props{c})};
            if (isnumeric (rgb))
              ## convert RGB color to RGB gray scale
              xfer = repmat ([0.30, 0.59, 0.11], rows (rgb), 1);
              ggg = repmat (sum (xfer .* rgb, 2), 1, 3);
              set (h(n), color_props{c}, ggg);
            endif
          endif
        endfor
      endfor
    endif

    if (! isempty (opts.font) || ! isempty (opts.fontsize))
      h = findall (opts.figure, "-property", "fontname");
      m = numel (props);
      for n = 1:numel (h)
        if (ishandle (h(n)))
          if (! isempty (opts.font))
            props(end+1).h = h(n);
            props(end).name = "fontname";
            props(end).value = {get(h(n), "fontname")};
          endif
        endif
        if (ishandle (h(n)))
          if (! isempty (opts.fontsize))
            props(end+1).h = h(n);
            props(end).name = "fontsize";
            props(end).value = {get(h(n), "fontsize")};
          endif
        endif
      endfor
      if (! isempty (opts.font))
        set (h(ishandle (h)), "fontname", opts.font);
      endif
      if (! isempty (opts.fontsize))
        if (ischar (opts.fontsize))
          fontsize = str2double (opts.fontsize);
        else
          fontsize = opts.fontsize;
        endif
        if (! isempty (opts.scalefontsize) && ! opts.scalefontsize != 1)
          ## This is done to work around the bbox being whole numbers.
          fontsize = fontsize * opts.scalefontsize;
        endif
        ## FIXME: legend child objects need to be acted on first.
        ##        or legend fontsize callback will destroy them.
        hlist = h(ishandle (h));
        haxes = strcmp (get (hlist, "type"), "axes");
        set (hlist(! haxes), "fontsize", fontsize);
        set (hlist(haxes), "fontsize", fontsize);
      endif
    endif

    ## call the graphics toolkit print script
    switch (get (opts.figure, "__graphics_toolkit__"))
      case "gnuplot"
        opts = __gnuplot_print__ (opts);
      otherwise
        opts = __opengl_print__ (opts);
    endswitch

  unwind_protect_cleanup
    ## restore modified properties
    if (isstruct (props))
      ## Restore figure position and units first
      for n = 2:-1:1
        if (ishandle (props(n).h))
          set (props(n).h, props(n).name, props(n).value{1});
        endif
      endfor
      for n = numel (props):-1:3
        if (ishandle (props(n).h))
          set (props(n).h, props(n).name, props(n).value{1});
        endif
      endfor
    endif

    ## Unlink temporary files
    for n = 1:numel (opts.unlink)
      [status, output] = unlink (opts.unlink{n});
      if (status != 0)
        warning ("print.m: %s, '%s'", output, opts.unlink{n});
      endif
    endfor
  end_unwind_protect

  if (isfigure (orig_figure))
    set (0, "currentfigure", orig_figure);
  endif

endfunction

function cmd = epstool (opts, filein, fileout)
  ## As epstool does not work with pipes, a subshell is used to
  ## permit piping. Since this solution does not work with the DOS
  ## command shell, the -tight and -preview options are disabled if
  ## output must be piped.

  ## DOS Shell:
  ##   gs.exe [...] -sOutputFile=<filein> - & epstool -bbox -preview-tiff <filein> <fileout> & del <filein>
  ## Unix Shell;
  ##   cat > <filein> ; epstool -bbox -preview-tiff <filein> <fileout> ; rm <filein>

  ## HACK: Keep track of whether ghostscript supports epswrite or eps2write.
  persistent epsdevice;
  if (isempty (epsdevice))
    [status, devlist] = system (sprintf ("%s -h", opts.ghostscript.binary));
    if (isempty (strfind (devlist, "eps2write")))
      epsdevice = "epswrite";
    else
      epsdevice = "eps2write";
    endif
  endif

  dos_shell = (ispc () && ! isunix ());

  cleanup = "";
  if (nargin < 3)
    fileout = opts.name;
  elseif (isempty (fileout))
    fileout = "-";
  endif

  if (nargin < 2 || strcmp (filein, "-") || isempty (filein))
    pipein = true;
    filein = [tempname() ".eps"];
    if (dos_shell)
      cleanup = sprintf ("& del %s ", strrep (filein, '/', '\'));
    else
      cleanup = sprintf ("; rm %s ", filein);
    endif
  else
    pipein = false;
    filein = strcat ("'", strtrim (filein), "'");
  endif
  if (strcmp (fileout, "-"))
    pipeout = true;
    fileout = [tempname() ".eps"];
    if (dos_shell)
      cleanup = [cleanup, sprintf("& del %s ", strrep (fileout, '/', '\'))];
    else
      cleanup = [cleanup, sprintf("; rm %s ", fileout)];
    endif
  else
    pipeout = false;
    fileout = strcat ("'", strtrim (fileout), "'");
  endif

  if (! isempty (opts.preview) && opts.tight_flag)
    warning ("print:previewandtight",
             "print.m: eps preview may not be combined with -tight");
  endif
  if (! isempty (opts.preview) || opts.tight_flag)
    if (! isempty (opts.epstool_binary))
      if (opts.tight_flag)
        cmd = "--copy --bbox";
      elseif (! isempty (opts.preview))
        switch (opts.preview)
          case "tiff"
            cmd = sprintf ("--add-%s-preview --device tiffg3", opts.preview);
          case {"tiff6u", "tiff6p", "metafile"}
            cmd = sprintf ("--add-%s-preview --device bmpgray", opts.preview);
          case {"tiff4", "interchange"}
            cmd = sprintf ("--add-%s-preview", opts.preview);
          case "pict"
            cmd = sprintf ("--add-%s-preview --mac-single", opts.preview);
          otherwise
            error ("print:invalidpreview",
                   "print.m: epstool cannot include preview for format '%s'",
                   opts.preview);
        endswitch
        if (! isempty (opts.ghostscript.resolution))
          cmd = sprintf ("%s --dpi %d", cmd, fix (opts.ghostscript.resolution));
        endif
      else
        cmd = "";
      endif
      if (! isempty (cmd))
        cmd = sprintf ("%s --quiet %s %s %s ", opts.epstool_binary,
                       cmd, filein, fileout);
      endif
      if (pipein)
        if (dos_shell)
          filein(filein=="'") = "\"";
          gs_cmd = __ghostscript__ ("binary", opts.ghostscript.binary,
                                    "device", epsdevice,
                                    "source", "-",
                                    "output", filein);
          cmd = sprintf ("%s %s & %s", gs_cmd, filein, cmd);
        else
          cmd = sprintf ("cat > %s ; %s", filein, cmd);
        endif
      endif
      if (pipeout)
        if (dos_shell)
          cmd = sprintf ("%s & type %s", cmd, fileout);
        else
          cmd = sprintf ("%s ; cat %s", cmd, fileout);
        endif
      endif
      if (! isempty (cleanup))
        if (pipeout && dos_shell)
          error ("print:epstoolpipe",
                 "print.m: cannot pipe output of 'epstool' for DOS shell");
        elseif (pipeout)
          cmd = sprintf ("( %s %s )", cmd, cleanup);
        else
          cmd = sprintf ("%s %s", cmd, cleanup);
        endif
      endif
    elseif (isempty (opts.epstool_binary))
      error ("print:noepstool", "print.m: 'epstool' not found in PATH");
    endif
  else
    if (pipein && pipeout)
      if (dos_shell)
        cmd = __ghostscript__ ("binary", opts.ghostscript.binary,
                               "device", epsdevice,
                               "source", "-",
                               "output", "-");
      else
        cmd = " cat ";
      endif
    elseif (pipein && ! pipeout)
      if (dos_shell)
        ## ghostscript expects double, not single, quotes
        fileout(fileout=="'") = "\"";
        cmd = __ghostscript__ ("binary", opts.ghostscript.binary,
                               "device", epsdevice,
                               "source", "-",
                               "output", fileout);
      else
        cmd = sprintf (" cat > %s ", fileout);
      endif
    elseif (! pipein && pipeout)
      if (dos_shell)
        cmd = sprintf (" type %s ", filein);
      else
        cmd = sprintf (" cat %s ", filein);
      endif
    else
      if (dos_shell)
        cmd = sprintf (" copy %s %s ", filein, fileout);
      else
        cmd = sprintf (" cp %s %s ", filein, fileout);
      endif
    endif
  endif
  if (opts.debug)
    fprintf ("epstool command: '%s'\n", cmd);
  endif
endfunction

function cmd = fig2dev (opts, devopt)
  if (nargin < 2)
    devopt = opts.devopt;
  endif
  dos_shell = (ispc () && ! isunix ());
  if (! isempty (opts.fig2dev_binary))
    if (dos_shell)
      ## FIXME: Is this the right thing to do for DOS?
      cmd = sprintf ("%s -L %s 2> NUL", opts.fig2dev_binary, devopt);
    else
      cmd = sprintf ("%s -L %s 2> /dev/null", opts.fig2dev_binary, devopt);
    endif
  elseif (isempty (opts.fig2dev_binary))
    error ("print:nofig2dev", "print.m: 'fig2dev' not found in PATH");
  endif
  if (opts.debug)
    fprintf ("fig2dev command: '%s'\n", cmd);
  endif
endfunction

function latex_standalone (opts)
  n = find (opts.name == ".", 1, "last");
  if (! isempty (n))
    opts.name = opts.name(1:n-1);
  endif
  latexfile = strcat (opts.name, ".tex");
  switch (opts.devopt)
    case {"pdflatexstandalone"}
      packages = "\\usepackage{graphicx,color}";
      graphicsfile = strcat (opts.name, "-inc.pdf");
    case {"pslatexstandalone"}
      packages = "\\usepackage{epsfig,color}";
      graphicsfile = strcat (opts.name, "-inc.ps");
    otherwise
      packages = "\\usepackage{epsfig,color}";
      graphicsfile = strcat (opts.name, "-inc.eps");
  endswitch
  papersize = sprintf ("\\usepackage[papersize={%.2fbp,%.2fbp},text={%.2fbp,%.2fbp}]{geometry}",
                       fix (opts.canvas_size), fix (opts.canvas_size));
  prepend = {"\\documentclass{minimal}";
             packages;
             papersize;
             "\\begin{document}";
             "\\centering"};
  postpend = {"\\end{document}"};
  fid = fopen (latexfile, "r");
  if (fid >= 0)
    latex = fscanf (fid, "%c", Inf);
    status = fclose (fid);
    if (status != 0)
      error ("print:errorclosingfile",
             "print.m: error closing file '%s'", latexfile);
    endif
    ## TODO - should this be fixed in GL2PS?
    latex = strrep (latex, "\\includegraphics{}",
                    sprintf ("\\includegraphics{%s}", graphicsfile));
  else
    error ("print:erroropeningfile",
           "print.m: error opening file '%s'", latexfile);
  endif
  fid = fopen (latexfile, "w");
  if (fid >= 0)
    fprintf (fid, "%s\n", prepend{:});
    fprintf (fid, "%s", latex);
    fprintf (fid, "%s\n", postpend{:});
    status = fclose (fid);
    if (status != 0)
      error ("print:errorclosingfile",
             "print.m: error closing file '%s'", latexfile);
    endif
  else
    error ("print:erroropeningfile",
           "print.m: error opening file '%s'", latexfile);
  endif
endfunction

function cmd = lpr (opts)
  if (nargin < 2)
    devopt = opts.devopt;
  endif
  if (! isempty (opts.lpr_binary))
    cmd = opts.lpr_binary;
    if (! isempty (opts.lpr_options))
      cmd = sprintf ("%s %s", cmd, opts.lpr_options);
    endif
    if (! isempty (opts.printer))
      cmd = sprintf ("%s %s", cmd, opts.printer);
    endif
  elseif (isempty (opts.lpr_binary))
    error ("print:nolpr", "print.m: 'lpr' not found in PATH");
  endif
  if (opts.debug)
    fprintf ("lpr command: '%s'\n", cmd);
  endif
endfunction

function cmd = pstoedit (opts, devopt)
  if (nargin < 2)
    devopt = opts.devopt;
  endif
  dos_shell = (ispc () && ! isunix ());
  if (! isempty (opts.pstoedit_binary))
    if (dos_shell)
      cmd = sprintf ("%s -f %s 2> NUL", opts.pstoedit_binary, devopt);
    else
      ## FIXME: Is this the right thing to do for DOS?
      cmd = sprintf ("%s -f %s 2> /dev/null", opts.pstoedit_binary, devopt);
    endif
  elseif (isempty (opts.pstoedit_binary))
    error ("print:nopstoedit", "print.m: 'pstoedit' not found in PATH");
  endif
  if (opts.debug)
    fprintf ("pstoedit command: '%s'\n", cmd);
  endif
endfunction

