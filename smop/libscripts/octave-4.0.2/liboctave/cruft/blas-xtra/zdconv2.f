c Copyright (C) 2010-2015  VZLU Prague, a.s., Czech Republic
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
      subroutine zdconv2o(ma,na,a,mb,nb,b,c)
c purpose:      a 2-dimensional outer additive convolution.
c               equivalent to the following:
c                 for i = 1:ma
c                   for j = 1:na
c                     c(i:i+mb-1,j:j+mb-1) += a(i,j)*b
c                   endfor
c                 endfor
c arguments:
c ma,na (in)    dimensions of a
c a (in)        1st matrix
c mb,nb (in)    dimensions of b
c b (in)        2nd matrix
c c (inout)     accumulator matrix, size (ma+mb-1, na+nb-1)
c
      integer ma,na,mb,nb
      double complex a(ma,na)
      double precision b(mb,nb)
      double complex c(ma+mb-1,na+nb-1)
      double complex btmp
      integer i,j,k
      external zaxpy
      do k = 1,na
        do j = 1,nb
          do i = 1,mb
            btmp = b(i,j)
            call zaxpy(ma,btmp,a(1,k),1,c(i,j+k-1),1)
          end do
        end do
      end do
      end subroutine

      subroutine zdconv2i(ma,na,a,mb,nb,b,c)
c purpose:      a 2-dimensional inner additive convolution.
c               equivalent to the following:
c                 for i = 1:ma-mb+1
c                   for j = 1:na-nb+1
c                     c(i,j) = sum (sum (a(i:i+mb-1,j:j+nb-1) .* b))
c                   endfor
c                 endfor
c arguments:
c ma,na (in)    dimensions of a
c a (in)        1st matrix
c mb,nb (in)    dimensions of b
c b (in)        2nd matrix
c c (inout)     accumulator matrix, size (ma+mb-1, na+nb-1)
c
      integer ma,na,mb,nb
      double complex a(ma,na)
      double precision b(mb,nb)
      double complex c(ma-mb+1,na-nb+1)
      double complex btmp
      integer i,j,k
      external zaxpy
      do k = 1,na-nb+1
        do j = 1,nb
          do i = 1,mb
            btmp = b(i,j)
            call zaxpy(ma-mb+1,btmp,a(mb+1-i,k+nb-j),1,c(1,k),1)
          end do
        end do
      end do
      end subroutine
