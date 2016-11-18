## Copyright (C) 2013-2015 Carnë Draug
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
## @deftypefn  {Function File} {} imformats ()
## @deftypefnx {Function File} {@var{formats} =} imformats (@var{ext})
## @deftypefnx {Function File} {@var{formats} =} imformats (@var{format})
## @deftypefnx {Function File} {@var{formats} =} imformats ("add", @var{format})
## @deftypefnx {Function File} {@var{formats} =} imformats ("remove", @var{ext})
## @deftypefnx {Function File} {@var{formats} =} imformats ("update", @var{ext}, @var{format})
## @deftypefnx {Function File} {@var{formats} =} imformats ("factory")
## Manage supported image formats.
##
## @var{formats} is a structure with information about each supported file
## format, or from a specific format @var{ext}, the value displayed on the
## field @code{ext}.  It contains the following fields:
##
## @table @asis
## @item ext
## The name of the file format.  This may match the file extension but Octave
## will automatically detect the file format.
##
## @item description
## A long description of the file format.
##
## @item @nospell{isa}
## A function handle to confirm if a file is of the specified format.
##
## @item write
## A function handle to write if a file is of the specified format.
##
## @item read
## A function handle to open files the specified format.
##
## @item info
## A function handle to obtain image information of the specified format.
##
## @item alpha
## Logical value if format supports alpha channel (transparency or matte).
##
## @item multipage
## Logical value if format supports multipage (multiple images per file).
## @end table
##
## It is possible to change the way Octave manages file formats with the
## options @qcode{"add"}, @qcode{"remove"}, and @qcode{"update"}, and supplying
## a structure @var{format} with the required fields.  The option
## @qcode{"factory"} resets the configuration to the default.
##
## This can be used by Octave packages to extend the image reading capabilities
## Octave, through use of the PKG_ADD and PKG_DEL commands.
##
## @seealso{imfinfo, imread, imwrite}
## @end deftypefn

## Author: Carnë Draug <carandraug@octave.org>

function varargout = imformats (arg1, arg2, arg3)
  if (nargin > 3)
    print_usage ();
  endif

  mlock (); # prevent formats to be removed by "clear all"
  persistent formats = default_formats ();

  if (nargin == 0 && nargout == 0)
    error ("imformats: pretty print not yet implemented.");
  elseif (nargin >= 1)
    if (isstruct (arg1))
      arrayfun (@is_valid_format, arg1);
      ## FIXME: what is the return value in this situation?
      formats = arg1;

    elseif (ischar (arg1))
      switch (tolower (arg1))
        case "add",
          if (! isstruct (arg2))
            error ("imformats: FORMAT to %s must be a structure.", arg1);
          endif
          arrayfun (@is_valid_format, arg2);
          formats(end + 1: end + numel (arg2)) = arg2;
          varargout{1} = formats;

        case {"remove", "update"},
          if (! ischar (arg2))
            error ("imformats: EXT to %s must be a string.", arg1);
          endif
          ## FIXME: suppose a format with multiple extensions. If one of
          ##        them is requested to be removed, should we remove the
          ##        whole format, or just that extension from the format?
          match = find_ext_idx (formats, arg2);
          if (! any (match))
            error ("imformats: no EXT `%s' found.", arg2);
          endif
          if (strcmpi (arg1, "remove"))
            formats(match) = [];
          else
            ## then it's update
            if (! isstruct (arg3))
              error ("imformats: FORMAT to update must be a structure.");
            endif
            is_valid_format (arg3);
            formats(match) = arg3;
          endif
          varargout{1} = formats;

        case "factory",
          formats = default_formats ();
        otherwise
          ## then we look for a format with that extension.
          match = find_ext_idx (formats, arg1);
          ## For matlab compatibility, if we don't find any format we must
          ## return an empty struct with NO fields. We can't use match as mask
          if (any (match))
            varargout{1} = formats(match);
          else
            varargout{1} = struct ();
          endif
      endswitch
    else
      error ("imformats: first argument must be either a structure or string.");
    endif
  else
    varargout{1} = formats;
  endif
endfunction

function formats = default_formats ()

  ## The available formats are dependent on what the user has installed at
  ## a given time, and how GraphicsMagick was built. Checking for
  ## GraphicsMagick features when building Octave is not enough since it
  ## delegates some of them to external programs which can be removed or
  ## installed at any time.
  ## The recommended method would be to use CoderInfoList() to get a list of
  ## all available coders and try to write and read back a small test image.
  ## But this will not work since some coders are readable or writable only.
  ## It will still fail if we test only the ones marked as readable and
  ## writable because some RW coders are not of image formats (NULL, 8BIM,
  ## or EXIF for example).
  ## So we'd need a blacklist (unacceptable because a `bad' coder may be
  ## added later) or a whitelist. A whitelist means that even with a
  ## super-fancy recent build of GraphicsMagick, some formats won't be listed
  ## by imformats but in truth, we will still be able to read and write them
  ## since imread() and imwrite() will give it a try anyway.
  ##
  ## For more info and comments from the GraphicsMagick main developer, see
  ## http://sourceforge.net/mailarchive/forum.php?thread_name=alpine.GSO.2.01.1304301916050.2267%40freddy.simplesystems.org&forum_name=graphicsmagick-help

  persistent formats = struct ( "coder", {},
                                "ext", {},
                                "isa", {},
                                "info", {},
                                "read", {},
                                "write", {},
                                "alpha", {},
                                "description", {},
                                "multipage", {});

  ## Image IO abilities won't change during the same Octave session,
  ## there's no need to go and calculate it all over again if we are
  ## requested to reset back to factory.
  if (! isempty (formats))
    return;
  endif

  ##      Building the formats info
  ##
  ## As mentioned above we start with a whitelist of coders. Since the
  ## GraphicsMagick build may be missing some coders, we will remove those
  ## from the list. Some info can be obtained directly from GraphicsMagick
  ## through the CoderInfo object. However, some will need to be hardcoded.
  ##
  ## The association between file extensions and coders needs to be done
  ## with a manually coded list (file extensions do not define the image
  ## format and GraphicsMagick will not be fooled by changing the extension).
  ##
  ## We can get the read, write, description and multipage fields from
  ## CoderInfo in C++. We should do the same for alpha (GraphicsMagick
  ## calls it matte) but it's not available from CoderInfo. The only way to
  ## check it is to create a sample image with each coder, then try to read
  ## it back with GraphicsMagick and use the matte method on the Image class.
  ## But making such test for each Octave session... meh! While technically
  ## it may be possible that the same coder has different support for alpha
  ## channel in different versions and builds, this doesn't seem to happen.
  ## So we also hardcode those. In the future, maybe the CoderInfo class will
  ## have a matte method like it does for multipage.
  ##
  ## Other notes: some formats have more than one coder that do the same. For
  ## example, for jpeg images there is both the JPG and JPEG coders. However,
  ## it seems that when reading images, GraphicsMagick only uses one of them
  ## and that's the one we list (it's the one reported by imfinfo and that we
  ## can use for isa). However, in some cases GraphicsMagick seems to rely
  ## uniquely on the file extension ((JBIG and JBG at least. Create an image
  ## with each of those coders, swap their extension and it will report the
  ## other coder). We don't have such cases on the whitelist but if we did, we
  ## would need two entries for such cases.

  ## each row: 1st => Coder, 2nd=> file extensions, 3rd=> alpha
  coders = {"BMP",  {"bmp"},          true;
            "CUR",  {"cur"},          false;
            "GIF",  {"gif"},          true;
            "ICO",  {"ico"},          true;
            "JBG",  {"jbg"},          false;
            "JBIG", {"jbig"},         false;
            "JP2",  {"jp2", "jpx"},   true;
            "JPEG", {"jpg", "jpeg"},  false; # there is also a JPG coder
            "PBM",  {"pbm"},          false;
            "PCX",  {"pcx"},          true;
            "PGM",  {"pgm"},          false;
            "PNG",  {"png"},          true;
            ## PNM is a family of formats supporting portable bitmaps (PBM),
            ## graymaps (PGM), and pixmaps (PPM). There is no file format
            ## associated with pnm itself. If PNM is used as the output format
            ## specifier, then GraphicsMagick automatically selects the most
            ## appropriate format to represent the image.
            "PNM",  {"pnm"},          true;
            "PPM",  {"ppm"},          false;
            "SUN",  {"ras"},          true; # SUN Rasterfile
            "TGA",  {"tga", "tpic"},  true;
            "TIFF", {"tif", "tiff"},  true;
            "XBM",  {"xbm"},          false;
            "XPM",  {"xpm"},          true;
            "XWD",  {"xwd"},          false;
            };

  for fidx = 1: rows(coders)
    formats(fidx).coder = coders{fidx, 1};
    formats(fidx).ext   = coders{fidx, 2};
    formats(fidx).alpha = coders{fidx, 3};
    ## default isa is to check if the format returned by imfinfo is the coder
    formats(fidx).isa   = @(x) isa_magick (coders{fidx,1}, x);
  endfor

  ## the default info, read, and write functions
  [formats.info ] = deal (@__imfinfo__);
  [formats.read ] = deal (@__imread__);
  [formats.write] = deal (@__imwrite__);

  ## fills rest of format information by checking with GraphicsMagick
  formats = __magick_formats__ (formats);
endfunction

function is_valid_format (format)
  ## the minimal list of fields required in the structure. We don't
  ## require multipage because it doesn't exist in matlab
  min_fields  = {"ext", "read", "isa", "write", "info", "alpha", "description"};
  fields_mask = isfield (format, min_fields);
  if (! all (fields_mask))
    error ("imformats: structure has missing field `%s'.", min_fields(! fields_mask){1});
  endif
endfunction

function match = find_ext_idx (formats, ext)
  ## XXX: what should we do if there's more than one hit?
  ##      Should this function prevent the addition of
  ##      duplicated extensions?
  match = cellfun (@(x) any (strcmpi (x, ext)), {formats.ext});
endfunction

function bool = isa_magick (coder, filename)
  bool = false;
  try
    info = __magick_ping__ (filename, 1);
    bool = strcmp (coder, info.Format);
  end_try_catch
endfunction

## When imread or imfinfo are called, the file must exist or the
## function defined by imformats will never be called.  Because
## of this, we must create a file for the tests to work.

## changing the function that does the reading
%!testif HAVE_MAGICK
%! fname = [tempname() ".jpg"];
%! def_fmt = imformats ();
%! fid = fopen (fname, "w");
%! unwind_protect
%!   fmt = imformats ("jpg");
%!   fmt.read = @numel;
%!   imformats ("update", "jpg", fmt);
%!   assert (imread (fname), numel (fname));
%! unwind_protect_cleanup
%!   fclose (fid);
%!   unlink (fname);
%!   imformats (def_fmt);
%! end_unwind_protect

## adding a new format
%!testif HAVE_MAGICK
%! fname = [tempname() ".new_fmt"];
%! def_fmt = imformats ();
%! fid = fopen (fname, "w");
%! unwind_protect
%!   fmt = imformats ("jpg"); # take jpg as template
%!   fmt.ext = "new_fmt";
%!   fmt.read = @() true ();
%!   imformats ("add", fmt);
%!   assert (imread (fname), true);
%! unwind_protect_cleanup
%!   fclose (fid);
%!   unlink (fname);
%!   imformats (def_fmt);
%! end_unwind_protect

## adding multiple formats at the same time
%!testif HAVE_MAGICK
%! fname1 = [tempname() ".new_fmt1"];
%! fid1 = fopen (fname1, "w");
%! fname2 = [tempname() ".new_fmt2"];
%! fid2 = fopen (fname2, "w");
%! def_fmt = imformats ();
%! unwind_protect
%!   fmt = imformats ("jpg"); # take jpg as template
%!   fmt.ext = "new_fmt1";
%!   fmt.read = @() true();
%!   fmt(2) = fmt(1);
%!   fmt(2).ext = "new_fmt2";
%!   imformats ("add", fmt);
%!   assert (imread (fname1), true);
%!   assert (imread (fname2), true);
%! unwind_protect_cleanup
%!   fclose (fid1);
%!   fclose (fid2);
%!   unlink (fname1);
%!   unlink (fname2);
%!   imformats (def_fmt);
%! end_unwind_protect

## changing format and resetting back to default
%!testif HAVE_MAGICK
%! ori_fmt = mod_fmt = imformats ("jpg");
%! mod_fmt.description = "Another description";
%! imformats ("update", "jpg", mod_fmt);
%! new_fmt = imformats ("jpg");
%! assert (new_fmt.description, mod_fmt.description);
%! imformats ("factory");
%! new_fmt = imformats ("jpg");
%! assert (new_fmt.description, ori_fmt.description);

## updating to an invalid format should cause an error
%!testif HAVE_MAGICK
%! fmt = imformats ("jpg");
%! fmt = rmfield (fmt, "read");
%! error_thrown = false;
%! try
%!   imformats ("update", "jpg", fmt);
%! catch
%!   error_thrown = true;
%! end_try_catch
%! assert (error_thrown, true);

