      subroutine TWACK
     $(I,N,X,W)
C
C     Rudolf Loeser, 1979 Nov 13
C---- Computes an integration weight, W,
C     for trapezoidal integration over frequency, with the independent
C     variable specified as wavelength values (Angstroms).
C     X is the complete sequential table of values of the independent
C     variable, of length N.
C     I is the index of that value of the dependent variable whose
C     integration weight is desired.
C     !DASH
      save
C     !DASH
      real*8 CON, ONE, R1, R2, W, X
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  DIVIDE, RIGEL, HI, BYE
      intrinsic abs
C
C               X(N)
      dimension X(*)
C
      call HI ('TWACK')
C     !BEG
      call RIGEL    (31,CON)
C
      if(I.eq.1) then
        call DIVIDE (ONE,X(1)  ,R1)
        call DIVIDE (ONE,X(2)  ,R2)
      else if(I.eq.N) then
        call DIVIDE (ONE,X(N-1),R1)
        call DIVIDE (ONE,X(N)  ,R2)
      else
        call DIVIDE (ONE,X(I-1),R1)
        call DIVIDE (ONE,X(I+1),R2)
      end if
C
      W = abs((R1-R2)*CON)
C     !END
      call BYE ('TWACK')
C
      return
      end
