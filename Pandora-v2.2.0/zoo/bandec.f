      subroutine BANDEC
     $(a,n,m1,m2,np,mp,al,mpl,indx,d)
C
C     from "Numerical Recipes," 1997 Aug 27
C     with only minimal adaptation to the Pandora environment
C     !DASH
      save
C     !DASH
      real*8 ONE, TINY, ZERO, a, al, d, dum
      integer i, indx, j, k, l, m1, m2, mm, mp, mpl, n, np
C     !DASH
      external  ABORT
      intrinsic abs
C
      parameter (TINY=1.e-20)
C
      dimension a(np,mp), al(np,mpl), indx(n)
C
      data ZERO,ONE /0.D0, 1.D0/
C
C     !BEG
      mm=m1+m2+1
      if(mm.gt.mp.or.m1.gt.mpl.or.n.gt.np) then
        write (*,100)
  100   format(' ','bad args in bandec')
        call ABORT
      end if
      l=m1
      do 13 i=1,m1
        do 11 j=m1+2-i,mm
          a(i,j-l)=a(i,j)
11      continue
        l=l-1
        do 12 j=mm-l,mm
          a(i,j)=ZERO
12      continue
13    continue
C     !EJECT
      d=ONE
      l=m1
      do 18 k=1,n
        dum=a(k,1)
        i=k
        if(l.lt.n)l=l+1
        do 14 j=k+1,l
          if(abs(a(j,1)).gt.abs(dum)) then
            dum=a(j,1)
            i=j
          end if
14      continue
        indx(k)=i
        if(dum.eq.ZERO) a(k,1)=TINY
        if(i.ne.k) then
          d=-d
          do 15 j=1,mm
            dum=a(k,j)
            a(k,j)=a(i,j)
            a(i,j)=dum
15        continue
        end if
        do 17 i=k+1,l
          dum=a(i,1)/a(k,1)
          al(k,i-k)=dum
          do 16 j=2,mm
            a(i,j-1)=a(i,j)-dum*a(k,j)
16        continue
          a(i,mm)=ZERO
17      continue
18    continue
C     !END
C
      return
      end
