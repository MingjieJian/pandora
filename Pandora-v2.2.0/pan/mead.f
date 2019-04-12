      subroutine MEAD
     $(EDGT,LEND,WLO,KLO,WHI,KHI,EDGE)
C
C     Rudolf Loeser, 1976 Feb 18
C---- Finds absorption edge, for AMBROSE.
C     !DASH
      save
C     !DASH
      real*8 EDGT, WHI, WLO, ZERO
      integer KHI, KLO, LEND
      logical EDGE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external NOTMORE, NOTLESS, HI, BYE
C
C               EDGT(LEND)
      dimension EDGT(*)
C
      call HI ('MEAD')
C     !BEG
      call NOTMORE (EDGT, LEND, WLO, KLO)
      if(KLO.eq.0) then
        KLO = 1
      end if
C
      call NOTLESS (EDGT, LEND, WHI, KHI)
      if(KHI.eq.0) then
        KHI = LEND+1
      end if
C
      EDGE = (KHI-KLO).gt.1
C     !END
      call BYE ('MEAD')
C
      return
      end
