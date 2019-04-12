      subroutine BANBKS
     $(a,n,m1,m2,np,mp,al,mpl,indx,b)
C
C     from "Numerical Recipes," 1997 Aug 27
C     with only minimal adaptation to the Pandora environment
C     !DASH
      save
C     !DASH
      real*8 a, al, b, dum
      integer i, indx, k, l, m1, m2, mm, mp, mpl, n, np
C     !DASH
      external ABORT
C
      dimension a(np,mp), al(np,mpl), b(n), indx(n)
C
C     !BEG
      mm=m1+m2+1
      if(mm.gt.mp.or.m1.gt.mpl.or.n.gt.np) then
        write (*,100)
  100   format(' ','bad args in banbks')
        call ABORT
      end if
      l=m1
      do 12 k=1,n
        i=indx(k)
        if(i.ne.k) then
          dum=b(k)
          b(k)=b(i)
          b(i)=dum
        end if
        if(l.lt.n)l=l+1
        do 11 i=k+1,l
          b(i)=b(i)-al(k,i-k)*b(k)
11      continue
12    continue
      l=1
      do 14 i=n,1,-1
        dum=b(i)
        do 13 k=2,l
          dum=dum-a(i,k)*b(k+i-1)
13      continue
        b(i)=dum/a(i,1)
        if(l.lt.mm) l=l+1
14    continue
C     !END
C
      return
      end
