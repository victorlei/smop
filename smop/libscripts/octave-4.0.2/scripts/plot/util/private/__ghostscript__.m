## Copyright (C) 2010-2015 Ben Abbott
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
## @deftypefn {Function File} {} __ghostscript__ (@var{@dots{}})
## Undocumented internal function.
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2010-07-26

function [gs_cmd, cleanup_cmd] = __ghostscript__ (varargin);

  dos_shell = (ispc () && ! isunix ());

  opts.binary = "";
  opts.source = "-";
  opts.output = "-";
  opts.device = "";
  opts.epscrop = false;
  opts.antialiasing = false;
  opts.antialiasing_textalphabits = 4;,
  opts.antialiasing_graphicsalphabits = 4;
  opts.resolution = 150;
  opts.papersize = "";
  opts.pageoffset = [0 0];
  opts.debug = false;
  opts.level = [];
  opts.prepend = "";

  offsetfile = "";
  offset_ps = {};
  cleanup_cmd = "";

  args = varargin;
  n = find (cellfun ("isclass", args, "struct"));
  if (! isempty (n))
    f = fieldnames (args{n});
    for m = 1:numel (f)
      opts.(f{m}) = args{n}.(f{m});
    endfor
    args(n) = [];
  endif
  for n = 1:2:numel (args)
    opts.(args{n}) = args{n+1};
  endfor

  if (isempty (opts.papersize))
    format_for_printer = false;
  else
    format_for_printer = true;
  endif

  gs_opts = ["-dQUIET -dNOPAUSE -dBATCH -dSAFER -dAutoRotatePages=/None -sDEVICE=" opts.device];

  if (! isempty (opts.level) && ismember (opts.level, [1, 2, 3]))
    gs_opts = sprintf ("%s -dLanguageLevel=%d", gs_opts, opts.level);
  endif

  if (opts.antialiasing && isempty (strfind (opts.device, "write")))
    ## Apply anti-aliasing to all bitmap formats/devices
    gs_opts = sprintf ("%s -dTextAlphaBits=%d -dGraphicsAlphaBits=%d",
                       gs_opts, fix (opts.antialiasing_textalphabits),
                       fix (opts.antialiasing_graphicsalphabits));
    gs_opts = sprintf ("%s -r%dx%d", gs_opts, fix ([1, 1] * opts.resolution));
  elseif (any (strcmp (opts.device, {"pswrite", "ps2write", "pdfwrite"})))
    gs_opts = sprintf ("%s -dEmbedAllFonts=true", gs_opts);
    if (strcmp (opts.device, "pdfwrite"))
      ## Optimize for loading
      gs_opts = sprintf ("%s -dOptimize=true", gs_opts);
    endif
  endif

  if (opts.epscrop)
    ## papersize is specified by the eps bbox
    gs_opts = sprintf ("%s -dEPSCrop", gs_opts);
  endif
  if (format_for_printer)
    if (ischar (opts.papersize))
      gs_opts = sprintf ("%s -sPAPERSIZE=%s", gs_opts, opts.papersize);
    elseif (isnumeric (opts.papersize) && numel (opts.papersize) == 2)
      gs_opts = sprintf ("%s -dDEVICEWIDTHPOINTS=%d -dDEVICEHEIGHTPOINTS=%d",
                         gs_opts, fix (opts.papersize));
      if (opts.papersize(1) > opts.papersize(2))
        ## Lanscape mode: This option will result in automatic rotation of the
        ##                document page if the requested page size matches one
        ##                of the default page sizes
        gs_opts = sprintf ("%s -dNORANGEPAGESIZE", gs_opts);
      endif
    else
      error ("print:badpapersize", "__ghostscript__.m: invalid 'papersize'");
    endif
    gs_opts = sprintf ("%s -dFIXEDMEDIA", gs_opts);
    ## "pageoffset" is relative to the coordinates, not the BBox LLHC.
    str = sprintf ("%s [%d %d] %s", "<< /Margins [0 0] /.HWMargins [0 0 0 0] /PageOffset",
                   fix (opts.pageoffset), ">> setpagedevice");
    offset_ps = {"%!PS-Adobe-3.0", str, "%%EOF"};
    if (isfield (opts, "offsetfile"))
      offsetfile = opts.offsetfile;
      cleanup_cmd = "";
    else
      offsetfile = [tempname() ".ps"];
      if (dos_shell)
        cleanup_cmd = ["del " strrep(offsetfile, '/', '\')];
      else
        cleanup_cmd = ["rm " offsetfile];
      endif
    endif
    unwind_protect
      fid = fopen (offsetfile, "w");
      if (fid == -1)
        error ("print:fopenfailed", "__ghostscript__.m: fopen () failed");
      endif
      fprintf (fid, "%s\n", offset_ps{:});
    unwind_protect_cleanup
      status = fclose (fid);
      if (status == -1)
        error ("print:fclosefailed", "__ghostscript__.m: fclose () failed");
      endif
    end_unwind_protect
    if (opts.debug)
      fprintf ("---- begin %s ----\n", offsetfile);
      fprintf ("%s\n", offset_ps{:});
      fprintf ("----- end %s -----\n", offsetfile);
    endif
  endif

  if (isempty (opts.binary))
    error ("print:no_ghostscript", "__ghostscript__.m: ghostscript is required.");
  elseif (isempty (opts.output))
    cmd = sprintf ("%s %s", opts.binary, gs_opts);
  else
    cmd = sprintf ("%s %s -sOutputFile=\"%s\"", opts.binary, gs_opts, opts.output);
  endif
  if (! isempty (opts.prepend)
      && any (strcmpi (opts.device, {"pswrite", "ps2write", "pdfwrite"})))
    ## FIXME: Fonts get may be mangled when appending ps/ps2.
    ##        See "How to concatenate several PS files" at the link,
    ##        http://en.wikibooks.org/wiki/PostScript_FAQ
    cmd = sprintf ("%s %s", cmd, opts.prepend);
    if (isempty (cleanup_cmd))
      if (dos_shell)
        cleanup_cmd = ["del " strrep(opts.prepend, '/', '\')];
      else
        cleanup_cmd = ["rm " opts.prepend];
      endif
    else
      if (dos_shell)
        cleanup_cmd = sprintf ("%s & del %s", cleanup_cmd,
                               strrep (opts.prepend, '/', '\'));
      else
        cleanup_cmd = sprintf ("%s ; rm %s", cleanup_cmd, opts.prepend);
      endif
    endif
  endif
  if (! isempty (offsetfile) && format_for_printer)
    cmd = sprintf ("%s %s", cmd, offsetfile);
  endif
  if (! isempty (opts.source))
    cmd = sprintf ("%s %s", cmd, opts.source);
  endif

  if (opts.debug)
    fprintf ("Ghostscript command: '%s'\n", cmd);
  endif

  gs_cmd = cmd;

endfunction


