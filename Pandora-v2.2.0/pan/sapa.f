      subroutine SAPA
     $(RGVL,N,NL,JQ,JX,GMIN,GMAX)
C
C     Rudolf Loeser, 1998 Jun 22
C---- Gets plot limits for KANALE.
C     !DASH
      save
C     !DASH
      real*8 GMAX, GMIN, RGVL
      integer IMAX, IMIN, J, JQ, JX, N, NL
C     !DASH
      external  GUARD, HI, BYE
      intrinsic min, max
C
C               RGVL(N,NL)
      dimension RGVL(N,*)
C
      call HI ('SAPA')
C     !BEG
      call GUARD   (JQ,JX,RGVL(1,1),IMIN,IMAX)
      GMIN = RGVL(IMIN,1)
      GMAX = RGVL(IMAX,1)
C
      do 100 J = 2,NL
        call GUARD (JQ,JX,RGVL(1,J),IMIN,IMAX)
        GMIN = min(GMIN,RGVL(IMIN,J))
        GMAX = max(GMAX,RGVL(IMAX,J))
  100 continue
C     !END
      call BYE ('SAPA')
C
      return
      end
