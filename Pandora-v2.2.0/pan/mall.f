      subroutine MALL
     $(A,B,C,Y,S,CHK,N)
C
C     Rudolf Loeser, 1997 Nov 12
C---- Checks a tridiagonal matrix solution.
C     (This is version 2 of MALL.)
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CHK, S, XNUM, Y
      integer I, N
C     !DASH
      external DIVIDE, HI, BYE
C
C               A(N), B(N), C(N), Y(N), S(N), CHK(N)
      dimension A(*), B(*), C(*), Y(*), S(*), CHK(*)
C
      call HI ('MALL')
C     !BEG
      XNUM = B(1)*Y(1)+C(1)*Y(2)
      call DIVIDE   (XNUM,S(1),CHK(1))
C
      do 100 I = 2,(N-1)
        XNUM = A(I)*Y(I-1)+B(I)*Y(I)+C(I)*Y(I+1)
        call DIVIDE (XNUM,S(I),CHK(I))
  100 continue
C
      XNUM = A(N)*Y(N-1)+B(N)*Y(N)
      call DIVIDE   (XNUM,S(N),CHK(N))
C     !END
      call BYE ('MALL')
C
      return
      end
