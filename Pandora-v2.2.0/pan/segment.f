      subroutine SEGMENT
     $(J,N,Y,TI,TJ,F)
C
C     Rudolf Loeser, 1971 Sep 20
C---- Computes a quadratic segment.
C     !DASH
      save
C     !DASH
      real*8 F, ONE, TI, TIJ, TJ, Y, ZERO
      integer J, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, HI, BYE
C
      call HI ('SEGMENT')
C     !BEG
      if(J.le.1) then
        F = ONE
C
      else if(J.ge.N) then
        F = TI
C
      else if(TI.lt.TJ) then
        call DIVIDE (TI, TJ, TIJ)
        F = (ONE-TIJ)*(ONE-Y*TIJ)
C
      else
        F = ZERO
      end if
C     !END
      call BYE ('SEGMENT')
C
      return
      end
