      subroutine JOGI
     $(ICE,XC,KDDR,KDRX,GMA,KNZGM)
C
C     Rudolf Loeser, 1978 Dec 18
C---- Determines whether or not this transition uses:
C     a) the DDR table, b) the alternate DR formula, and
C     c) a non-zero GMMA, for PRD transitions,
C     for BUFFALO.
C     (This is version 3 of JOGI.)
C     !DASH
      save
C     !DASH
      real*8 GMA, XC, ZERO
      integer ICE, KDDR, KDRX, KNZGM
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
      call HI ('JOGI')
C     !BEG
      if(ICE.ne.0) then
C
        if(XC.lt.ZERO) then
          KDDR = KDDR+1
        else if (XC.eq.ZERO) then
          KDRX = KDRX+1
        end if
C
        if(GMA.ne.ZERO) then
          KNZGM = KNZGM+1
        end if
C
      end if
C     !END
      call BYE ('JOGI')
C
      return
      end
