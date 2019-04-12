      subroutine MALT
     $(N,DEL,FD,G,R,KBINN,KBOUT,A,B,C,D,E)
C
C     Rudolf Loeser, 1998 Jan 30
C---- Computes A, B, C, D, E, for IMPALA.
C     (This is version 6 of MALT.)
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, DEL, E, FD, G, R, ZERO
      integer I, KBINN, KBOUT, N
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
C               DEL(N), FD(N), G(N), R(N), A(N), B(N), C(N), D(N), E(N)
      dimension DEL(*), FD(*), G(*), R(*), A(*), B(*), C(*), D(*), E(*)
C     !EJECT
C
      call HI ('MALT')
C     !BEG
C---- For i = 1
      if(KBINN.eq.1) then
        A(1) =  ZERO
        B(1) =  ZERO
        C(1) =  R(1)
      else
        A(1) = -FD(2)/DEL(1)
        B(1) =  (G(2)+FD(1))/DEL(1)
        C(1) =  R(1)-(G(1)+FD(1)-FD(2))/DEL(1)
      end if
      D(1) = ZERO
      E(1) = ZERO
C
C---- For i = 2
      A(2) = -FD(3)/DEL(2)
      B(2) =  G(3)/DEL(2)
      C(2) =  R(2)+(FD(1)+FD(3))/DEL(2)
      D(2) = -(G(1)+FD(1))/DEL(2)
      E(2) =  ZERO
C
C---- For interior values of i
      do 100 I = 3,(N-2)
        A(I) = -FD(I+1)/DEL(I)
        B(I) =  G(I+1)/DEL(I)
        C(I) =  R(I)+(FD(I-1)+FD(I+1))/DEL(I)
        D(I) = -G(I-1)/DEL(I)
        E(I) = -FD(I-1)/DEL(I)
  100 continue
C
C---- For i = N-1
      A(N-1) =  ZERO
      B(N-1) =  (G(N)-FD(N))/DEL(N-1)
      C(N-1) =  R(N-1)+(FD(N-2)+FD(N))/DEL(N-1)
      D(N-1) = -G(N-2)/DEL(N-1)
      E(N-1) = -FD(N-2)/DEL(N-1)
C
C---- For i = N
      A(N) = ZERO
      B(N) = ZERO
      if(KBOUT.eq.1) then
        C(N) =  R(N)
        D(N) =  ZERO
        E(N) =  ZERO
      else
        C(N) =  R(N)+(G(N)-FD(N)+FD(N-1))/DEL(N)
        D(N) = -(G(N-1)-FD(N))/DEL(N)
        E(N) = -FD(N-1)/DEL(N)
      end if
C     !END
      call BYE ('MALT')
C
      return
      end
