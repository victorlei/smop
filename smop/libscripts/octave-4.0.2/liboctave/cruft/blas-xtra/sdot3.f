c Copyright (C) 2009-2015  VZLU Prague, a.s., Czech Republic
c
c Author: Jaroslav Hajek <highegg@gmail.com>
c
c This file is part of Octave.
c
c Octave is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 3 of the License, or
c (at your option) any later version.
c
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c
c You should have received a copy of the GNU General Public License
c along with this software; see the file COPYING.  If not, see
c <http://www.gnu.org/licenses/>.
c
      subroutine sdot3(m,n,k,a,b,c)
c purpose:      a 3-dimensional dot product.
c               c = sum (a .* b, 2), where a and b are 3d arrays.
c arguments:
c m,n,k (in)    the dimensions of a and b
c a,b (in)      real input arrays of size (m,k,n)
c c (out)       real output array, size (m,n)
      integer m,n,k,i,j,l
      real a(m,k,n),b(m,k,n)
      real c(m,n)

      real sdot
      external sdot

c quick return if possible.
      if (m <= 0 .or. n <= 0) return

      if (m == 1) then
c the column-major case.
        do j = 1,n
          c(1,j) = sdot(k,a(1,1,j),1,b(1,1,j),1)
        end do
      else
c We prefer performance here, because that's what we generally
c do by default in reduction functions. Besides, the accuracy
c of xDOT is questionable. Hence, do a cache-aligned nested loop.
        do j = 1,n
          do i = 1,m
            c(i,j) = 0d0
          end do
          do l = 1,k
            do i = 1,m
              c(i,j) = c(i,j) + a(i,l,j)*b(i,l,j)
            end do
          end do
        end do
      end if

      end subroutine
