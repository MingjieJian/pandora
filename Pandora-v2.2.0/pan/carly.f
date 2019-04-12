      subroutine CARLY
     $(EP,AW,XJBAR,BS,CHI)
C
C     Rudolf Loeser, 2003 Apr 11
C---- Computes CHI.
C     !DASH
      save
C     !DASH
      real*8 AW, BS, CHI, EP, ONE, XDEN, XJBAR, XNUM
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, HI, BYE
C
      call HI ('CARLY')
C     !BEG
      XNUM = XJBAR*(EP-AW)-(ONE+AW)*(EP*BS)
      XDEN = ONE+EP
      call DIVIDE (XNUM, XDEN, CHI)
C     !END
      call BYE ('CARLY')
C
      return
      end
