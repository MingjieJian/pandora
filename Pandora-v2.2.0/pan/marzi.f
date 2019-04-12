      subroutine MARZI
     $(MF,K,YNT,WMU,WINT)
C
C     Rudolf Loeser, 2000 Jul 20
C---- Accumulates a weighted intensity profile (needed for subsequent
C     calculation of a flux profile).
C     !DASH
      save
C     !DASH
      real*8 WINT, WMU, YNT
      integer K, MF
C     !DASH
      external ZERO1, ARRINC, HI, BYE
C
C               YNT(KM), WINT(KM), WMU(LF)
      dimension YNT(*),  WINT(*),  WMU(*)
C
      call HI ('MARZI')
C     !BEG
      if(MF.eq.1) then
C----   Initialize the flux sum
        call ZERO1 (WINT,K)
      end if
C---- Add current, weighted contribution
      call ARRINC  (YNT,WMU(MF),WINT,K)
C     !END
      call BYE ('MARZI')
C
      return
      end
