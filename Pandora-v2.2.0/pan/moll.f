      subroutine MOLL
     $(A,B,C,D,Y,S,CHK,N)
C
C     Rudolf Loeser, 1997 Nov 12
C---- Checks a fourdiagonal matrix solution.
C     (This is version 2 of MOLL.)
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CHK, D, S, XNUM, Y
      integer I, N
C     !DASH
      external DIVIDE, HI, BYE
C
C               A(N), B(N), C(N), D(N), Y(N), S(N), CHK(N)
      dimension A(*), B(*), C(*), D(*), Y(*), S(*), CHK(*)
C
      call HI ('MOLL')
C     !BEG
      XNUM = B(1)*Y(1)+A(1)*Y(2)
      call DIVIDE   (XNUM,S(1),CHK(1))
C
      XNUM = C(2)*Y(1)+B(2)*Y(2)+A(2)*Y(3)
      call DIVIDE   (XNUM,S(2),CHK(2))
C
      do 100 I = 3,(N-1)
        XNUM = D(I)*Y(I-2)+C(I)*Y(I-1)+B(I)*Y(I)+A(I)*Y(I+1)
        call DIVIDE (XNUM,S(I),CHK(I))
  100 continue
C
      XNUM = D(N)*Y(N-2)+C(N)*Y(N-1)+B(N)*Y(N)
      call DIVIDE   (XNUM,S(N),CHK(N))
C     !END
      call BYE ('MOLL')
C
      return
      end
