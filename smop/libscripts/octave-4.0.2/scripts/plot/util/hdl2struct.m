## Copyright (C) 2012-2015 pdiribarne
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{s} =} hdl2struct (@var{h})
## Return a structure, @var{s}, whose fields describe the properties
## of the object, and its children, associated with the handle, @var{h}.
##
## The fields of the structure @var{s} are @qcode{"type"}, @qcode{"handle"},
## @qcode{"properties"}, @qcode{"children"}, and @qcode{"special"}.
## @seealso{struct2hdl, hgsave, findobj}
## @end deftypefn

## Author: pdiribarne <pdiribarne@new-host.home>
## Created: 2012-03-04

function s = hdl2struct (h)

  if (nargin != 1 || ! ishandle (h))
    print_usage ();
  endif

  hiddenh = get (0, "showhiddenhandles");
  if (strcmp (hiddenh, "on"))
    set (0, "showhiddenhandles", "off");
  endif

  ## main object
  main = get (h);
  s.handle = h;
  s.type = main.type;
  s.properties = getprops (h);
  s.children = [];
  s.special = [];

  ## sweep all children but legends, colorbars, uimenu and hggroup children
  ## in reverse order
  kids = main.children;
  lg = findobj (h, "-depth", 1, "tag", "legend");
  cb = findobj (h, "-depth", 1, "tag", "colorbar");
  ui = findobj (h, "-depth", 1, "type", "uimenu");
  nkids = length (kids);
  ii = 0;
  while (nkids)
    if (! any (kids (nkids) == lg) && ! any (kids (nkids) == cb)
          && ! any (kids (nkids) == ui) && ! strcmp (main.type, "hggroup"))
      ii++;
      s.children(ii) = hdl2struct (kids(nkids));
    endif
    nkids--;
  endwhile

  ## add non "children" children objects (title, xlabel, ...) and
  ## hggroup children and tag theim in "special"
  special = [];
  if (strcmp (main.type, "hggroup"))
    special = main.children;
  endif
  special = [special getspecial(h)];
  nsp = length (special);
  while (nsp)
    ii++;
    s.children(ii) = hdl2struct (special(nsp));
    s.special(nsp) = ii;
    nsp--;
  endwhile

  ## look for legends and colorbars among "main"'s brothers and add them
  ## to the children list
  if (strcmp (main.type, "axes"))
    par = main.parent;
    lg = findobj (par, "-depth", 1, "tag", "legend");
    if (! isempty (lg))
      idx = arrayfun (@(x) get(x).userdata.handle(end) == h, lg);
      lg = lg(find (idx));
    endif
    nlg = length (lg);
    if (nlg == 1)
      ii++;
      s.children(ii) = hdl2struct (lg);
    elseif (nlg > 1)
      error ("hdl2struct: more than one legend found");
    endif

    cb = findobj (par, "-depth", 1, "tag", "colorbar");
    if (! isempty (cb))
      idx = arrayfun (@(x) get(x).axes == h, cb);
      cb = cb(find (idx));
    endif

    ncb = length (cb);
    if (ncb == 1)
      ii++;
      s.children(ii) = hdl2struct (cb);
    elseif (nlg > 1)
      error ("hdl2struct: more than one colorbar found");
    endif
  endif

  set (0, "showhiddenhandles", hiddenh);

endfunction

function hdlist = getspecial (h)
  obj = get (h);
  ## return handles to special children
  fields = fieldnames (obj);
  hdlist = [];

  regkids = get ( h, "children");
  set (0, "showhiddenhandles", "on");
  allkids = get ( h, "children");
  set (0, "showhiddenhandles", "off");
  speckids = arrayfun (@(x) ! any (x == regkids), allkids);
  hdlist = allkids (find (speckids));
  hdlist = reshape (hdlist, 1, numel (hdlist));

endfunction

function prpstr = getprops (h)
  obj = get (h);
  ## get usefull properties rejecting readonly, children, handles ...
  fields = fieldnames (obj);
  hdlist = [];

  forbid = {"beingdeleted", "busyaction", "buttondownfcn", ...
            "children", "clipping", "createfcn", ...
            "deletefcn", "handlevisibility", "hittest", ...
            "interruptible", "parent", "selected" ,...
            "selectionhighlight", "type", "__modified__", ...
            "uicontextmenu", "__graphics_toolkit__", "currentaxes", ...
            "currentcharacter", "currentobject","tightinset", ...
            "currentpoint", "extent"};

  nflds = length (fields);
  ii = 0;
  while (nflds)
    prop = fields{nflds};
    val = obj.(fields{nflds});
    ii++;
    if (! any (strcmp (prop, forbid)))
      prpstr.(prop) = val;
    endif
    nflds--;
  endwhile

  ## hidden properties
  hidden = {"autopos_tag", "looseinset"};
  for ii = 1:numel (hidden)
    if (isprop (h, hidden{ii}))
      prpstr.(hidden{ii}) = get (h, hidden{ii});
    endif
  endfor

endfunction


## FIXME: need validation tests

