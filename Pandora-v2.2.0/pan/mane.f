      subroutine MANE
     $(NL,J,NSL,F)
C
C     Rudolf Loeser, 1978 May 04
C---- Computes a linear interpolation coefficient, for PARASO.
C     !DASH
      save
C     !DASH
      real*8 A, B, F, ONE
      integer J, NL, NSL
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
      call HI ('MANE')
C     !BEG
      if(J.ge.NSL) then
        F = ONE
      else
        A = J-NL
        B = NSL-NL
        call DIVIDE (A,B,F)
      end if
C     !END
      call BYE ('MANE')
C
      return
      end
