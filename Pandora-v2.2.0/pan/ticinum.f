      subroutine TICINUM
     $(C,N,XA,XJNU,SUM)
C
C     Rudolf Loeser, 1982 Feb 02
C---- Accumulates an angle contribution, for CSF-calculation.
C     !DASH
      save
C     !DASH
      real*8 C, SUM, XA, XJNU
      integer I, N
C     !DASH
      external HI, BYE
C
C               XA(N,N), XJNU(N), C(N), SUM(N)
      dimension XA(N,*), XJNU(*), C(*), SUM(*)
C
      call HI ('TICINUM')
C     !BEG
      do 100 I = 1,N
        SUM(I) = SUM(I)+C(I)*XA(I,I)*XJNU(I)
  100 continue
C     !END
      call BYE ('TICINUM')
C
      return
      end
