      subroutine SWART
     $(XLMTHR,NP,XLM,RCP,WAVE,NW)
C
C     Rudolf Loeser, 2004 Aug 11
C---- Finds an "absorption edge".
C     (This is version 2 of SWART.)
C     !DASH
      save
C     !DASH
      real*8 RCP, WAVE, XLM, XLMTHR, ZERO
      integer NP, NW
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
C               XLM(NDPNT), RCP(NDPNT), WAVE(2)
      dimension XLM(*),     RCP(*),     WAVE(*)
C
C
      call HI ('SWART')
C     !BEG
      NW = 1
      if((NP.gt.2).and.(RCP(NP).eq.ZERO).and.(RCP(NP-2).gt.ZERO)) then
        NW = 2
      end if
C
      if(NW.eq.1) then
        WAVE(1) = XLMTHR
      else
        WAVE(1) = XLM(NP-1)
        WAVE(2) = XLM(NP-2)
      end if
C     !END
      call BYE ('SWART')
C
      return
      end
