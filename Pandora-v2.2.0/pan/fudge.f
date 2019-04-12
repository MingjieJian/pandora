      subroutine FUDGE
     $(XLM,JM,XLMM,XMLC,FFAC)
C
C     Rudolf Loeser, 1971 Sep 16
C---- Obtains a background opacity multiplier from the table.
C     !DASH
      save
C     !DASH
      real*8 FFAC, ONE, XLM, XLMM, XMLC
      integer JM, jummy
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external LININT, HI, BYE
C
C               XLMM(JM), XMLC(JM)
      dimension XLMM(*),  XMLC(*)
C
      call HI ('FUDGE')
C     !BEG
      if(JM.le.0) then
        FFAC = ONE
C
      else if(JM.eq.1) then
        FFAC = XMLC(1)
C
      else if((XLM.lt.XLMM(1)).or.(XLM.gt.XLMM(JM))) then
        FFAC = ONE
C
      else
        call LININT (XLMM, 1, XMLC, 1, JM, XLM, FFAC, 1, 1, jummy)
      end if
C     !END
      call BYE ('FUDGE')
C
      return
      end
