      subroutine CHELA
     $(X,Y,N,A)
C
C     Rudolf Loeser, 1983 Oct 28
C---- Trapezoidal rule averaging.
C     !DASH
      save
C     !DASH
      real*8 A, HALF, S, X, Y, ZERO
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
C     !DASH
      external HI, BYE
C
C               X(N), Y(N)
      dimension X(*), Y(*)
C
      call HI ('CHELA')
C     !BEG
      if(N.gt.1) then
        S = ZERO
        do 100 I = 1,(N-1)
          S = S+((Y(I+1)+Y(I))*(X(I+1)-X(I)))
  100   continue
        S = HALF*S
        A = S/(X(N)-X(1))
      else
        A = Y(1)
      end if
C     !END
      call BYE ('CHELA')
C
      return
      end
