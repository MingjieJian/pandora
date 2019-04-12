      subroutine HENBIT
     $(XLMTAB,RCPTAB,NP,XLM,RCP)
C
C     Rudolf Loeser, 1988 Apr 28
C---- Computes RCP, for HONEY.
C     (This is version 2 of HENBIT.)
C     !DASH
      save
C     !DASH
      real*8 FAC, RCP, RCPTAB, XLM, XLMTAB
      integer NP, jummy
C     !DASH
      external LININT, HI, BYE
C
C               XLMTAB(NP), RCPTAB(NP)
      dimension XLMTAB(*),  RCPTAB(*)
C
      call HI ('HENBIT')
C     !BEG
      if(XLM.lt.XLMTAB(1)) then
C
        FAC = (XLM/XLMTAB(1))**3
        RCP = RCPTAB(1)*FAC
C
      else
C
        call LININT (XLMTAB, 1, RCPTAB, 1, NP, XLM, RCP, 1, 1, jummy)
C
      end if
C     !END
      call BYE ('HENBIT')
C
      return
      end
