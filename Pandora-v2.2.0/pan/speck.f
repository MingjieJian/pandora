      subroutine SPECK
     $(CHECK,DELTA,PLOT)
C
C     Rudolf Loeser, 1968 Jul 26
C---- Processes a point to be plotted in the CHECK graph,
C     assuring that -3 .le. log(abs(DELTA)) .le. 0,
C     where DELTA=CHECK-1.
C     !DASH
      save
C     !DASH
      real*8 CHECK, DELTA, ONE, PLOT, THREE, ZERO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 4),THREE )
C     !DASH
      external  HI, BYE
      intrinsic abs, max, min
C
      call HI ('SPECK')
C     !BEG
      DELTA = CHECK-ONE
      if(DELTA.ne.ZERO) then
        PLOT = log10(abs(DELTA))
        PLOT = max(min(PLOT,ZERO),-THREE)
      else
        PLOT = -THREE
      end if
C     !END
      call BYE ('SPECK')
C
      return
      end
