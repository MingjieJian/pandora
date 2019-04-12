      subroutine SOUFFLE
     $(IS,IE,COP,GTN,PHI,BC,S,XKPNU,SNU)
C
C     Rudolf Loeser, 1981 Nov 09
C---- Computes the Monochromatic Source Function, SNU.
C     (This is version 4 of SOUFFLE.)
C     !DASH
      save
C     !DASH
      real*8 BC, COP, GTN, PHI, S, SNU, XKPNU
      integer I, IE, IS, KOUNT
C     !DASH
      external ARRDIV, HI, BYE
C
      dimension COP(*), GTN(*), PHI(*), BC(*), S(*), XKPNU(*), SNU(*)
C
      call HI ('SOUFFLE')
C     !BEG
      KOUNT = 0
      do 100 I = IS,IE
        KOUNT  = KOUNT+1
        SNU(I) = GTN(I)*PHI(I)*S(I)+COP(I)*BC(I)
  100 continue
      call ARRDIV (SNU(IS),XKPNU(IS),SNU(IS),KOUNT)
C     !END
      call BYE ('SOUFFLE')
C
      return
      end
