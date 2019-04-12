      subroutine LOBBY
     $(A,B,C,D,E,Y,S,CHK,N)
C
C     Rudolf Loeser, 1998 Jan 30
C---- Checks a fivediagonal matrix solution.
C     (This is version 6 of LOBBY.)
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CHK, D, E, S, XNUM, Y
      integer I, N
C     !DASH
      external DIVIDE, HI, BYE
C
C               A(N), B(N), C(N), D(N), E(N), Y(N), S(N), CHK(N)
      dimension A(*), B(*), C(*), D(*), E(*), Y(*), S(*), CHK(*)
C
      call HI ('LOBBY')
C     !BEG
      XNUM = C(1)*Y(1)+B(1)*Y(2)+A(1)*Y(3)
      call DIVIDE   (XNUM,S(1),CHK(1))
C
      XNUM = D(2)*Y(1)+C(2)*Y(2)+B(2)*Y(3)+A(2)*Y(4)
      call DIVIDE   (XNUM,S(2),CHK(2))
C
      do 100 I = 3,(N-2)
        XNUM = E(I)*Y(I-2)+D(I)*Y(I-1)+C(I)*Y(I)+B(I)*Y(I+1)+A(I)*Y(I+2)
        call DIVIDE (XNUM,S(I),CHK(I))
  100 continue
C
      XNUM = E(N-1)*Y(N-3)+D(N-1)*Y(N-2)+C(N-1)*Y(N-1)+B(N-1)*Y(N)
      call DIVIDE   (XNUM,S(N-1),CHK(N-1))
C
      XNUM = E(N)*Y(N-2)+D(N)*Y(N-1)+C(N)*Y(N)
      call DIVIDE   (XNUM,S(N),CHK(N))
C     !END
      call BYE ('LOBBY')
C
      return
      end
