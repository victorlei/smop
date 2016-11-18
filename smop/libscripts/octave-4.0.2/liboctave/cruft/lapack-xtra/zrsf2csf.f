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

       subroutine zrsf2csf(n,t,u,c,s)
       integer n
       double complex t(n,n),u(n,n)
       double precision c(n-1),s(n-1)
       double precision x,y,z
       integer j
       do j = 1,n-1
          c(j) = 1
       end do
       j = 1
       do while (j < n)
c apply previous rotations to rows
         call zrcrot1(j,t(1,j),c,s)

         y = t(j+1,j)
         if (y /= 0) then
c 2x2 block, form Givens rotation [c, i*s; i*s, c]
           z = t(j,j+1)
           c(j) = sqrt(z/(z-y))
           s(j) = sqrt(y/(y-z))
c apply new rotation to t(j:j+1,j)
           call zrcrot1(2,t(j,j),c(j),s(j))
c apply all rotations to t(1:j+1,j+1)
           call zrcrot1(j+1,t(1,j+1),c,s)
c apply new rotation to columns j,j+1
           call zrcrot2(j+1,t(1,j),t(1,j+1),c(j),s(j))
c zero subdiagonal entry, skip next row
           t(j+1,j) = 0
           j = j + 2
         else
           j = j + 1
         end if
       end do

c apply rotations to last column if needed
       if (j == n) then
         call zrcrot1(j,t(1,j),c,s)
       end if

c apply stored rotations to all columns of u
       do j = 1,n-1
         if (c(j) /= 1) then
           call zrcrot2(n,u(1,j),u(1,j+1),c(j),s(j))
         end if
       end do

       end subroutine

       subroutine zrcrot1(n,x,c,s)
c apply rotations to a column from the left
       integer n
       double complex x(n), t
       double precision c(n-1),s(n-1)
       integer i
       do i = 1,n-1
         if (c(i) /= 1) then
           t = x(i)*c(i) - x(i+1)*dcmplx(0,s(i))
           x(i+1) = x(i+1)*c(i) - x(i)*dcmplx(0,s(i))
           x(i) = t
         endif
       end do
       end subroutine

       subroutine zrcrot2(n,x,y,c,s)
c apply a single rotation from the right to a pair of columns
       integer n
       double complex x(n),y(n),t
       double precision c, s
       integer i
       do i = 1,n
         t = x(i)*c + y(i)*dcmplx(0,s)
         y(i) = y(i)*c + x(i)*dcmplx(0,s)
         x(i) = t
       end do
       end subroutine





