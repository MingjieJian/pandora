      subroutine LAPEL
     $(WVL,CRIT,XLM,XIA,K)
C
C     Rudolf Loeser, 2004 Apr 26
C---- Adds a value to XIA.
C     (This is version 3 of LAPEL.)
C     !DASH
      save
C     !DASH
      real*8 CRIT, WVL, XIA, XLM
      integer IFLG, K
C     !DASH
      external COMPD, HI, BYE
C
C               XIA(KM)
      dimension XIA(*)
C
      call HI ('LAPEL')
C     !BEG
      call COMPD (WVL, XLM, CRIT, IFLG)
      if(IFLG.ne.0) then
        K = K+1
        XIA(K) = XLM
      end if
C     !END
      call BYE ('LAPEL')
C
      return
      end
