      subroutine KRAKEN
     $(NO,I,B1,B2,B3,BDSTAR,W,R1,R2,R3,RHOSTAR,CHI,S,YBAR)
C
C     Rudolf Loeser, 1972 Jun 21
C---- Sets up and prints a line for CROCUS.
C     !DASH
      save
C     !DASH
      real*8 B, B1, B2, B3, BDSTAR, CHI, R, R1, R2, R3, RHOSTAR, S, W,
     $       YBAR, ZERO
      integer I, J, NO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  SCALP, HI, BYE
      intrinsic mod
C
      dimension B(3), R(3)
C
      call HI ('KRAKEN')
C     !BEG
      J = mod(I,1000)
C
      call SCALP (B1, BDSTAR , B(1))
      call SCALP (B2, BDSTAR , B(2))
      call SCALP (B3, BDSTAR , B(3))
C
      call SCALP (R1, RHOSTAR, R(1))
      call SCALP (R2, RHOSTAR, R(2))
      call SCALP (R3, RHOSTAR, R(3))
C
      write (NO,100) J,B,BDSTAR,W,J,R,RHOSTAR,CHI,S,YBAR
  100 format(' ',I3,3F9.5,1PE13.5,E10.1,1X,I3,0P3F9.5,1PE13.5,3E10.2)
C     !END
      call BYE ('KRAKEN')
C
      return
      end
