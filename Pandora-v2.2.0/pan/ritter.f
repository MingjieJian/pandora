      subroutine RITTER
     $(MX,LX,LDL,DDL,WLO,WHI)
C
C     Rudolf Loeser, 2007 Jan 29
C---- Augments WLO and WHI for blended background lines.
C     !DASH
      save
C     !DASH
      real*8 DDL, WHI, WLO, ZERO
      integer L, LDL, LX, M, MX
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
C               LDL(MX), WLO(MX), WHI(MX), DDL(LX,MX)
      dimension LDL(*),  WLO(*),  WHI(*),  DDL(LX,*)
C
      call HI ('RITTER')
C     !BEG
      do 100 M = 1,MX
        if(DDL(1,M).lt.ZERO) then
          WLO(M) = WLO(M)+DDL(1,M)
        end if
        L = LDL(M)
        if(DDL(L,M).gt.ZERO) then
          WHI(M) = WHI(M)+DDL(L,M)
        end if
  100 continue
C     !END
      call BYE ('RITTER')
C
      return
      end
