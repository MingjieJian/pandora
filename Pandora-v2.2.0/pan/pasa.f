      subroutine PASA
     $(GVL,N,NS,NL,JQ,JX,GMIN,GMAX)
C
C     Rudolf Loeser, 1990 Apr 23
C---- Gets plot limits for LAKANE.
C     !DASH
      save
C     !DASH
      real*8 GMAX, GMIN, GVL
      integer IMAX, IMIN, J, JQ, JX, N, NL, NS
C     !DASH
      external  GUARD, HI, BYE
      intrinsic min, max
C
C               GVL(N,NL)
      dimension GVL(N,*)
C
C
      call HI ('PASA')
C     !BEG
      call GUARD   (JQ, JX, GVL(1,NS), IMIN, IMAX)
      GMIN = GVL(IMIN,NS)
      GMAX = GVL(IMAX,NS)
C
      do 100 J = (NS+1),NL
        call GUARD (JQ, JX, GVL(1,J),  IMIN, IMAX)
        GMIN = min(GMIN,GVL(IMIN,J))
        GMAX = max(GMAX,GVL(IMAX,J))
  100 continue
C     !END
      call BYE ('PASA')
C
      return
      end
