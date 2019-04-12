      subroutine SARDINE
     $(XK,N,U,EMUK)
C
C     Rudolf Loeser, 2006 Oct 31
C---- Computes the table EMUK(i) = exp(-U(i)*XK),
C     for Lyman calculations.
C     !DASH
      save
C     !DASH
      real*8 EMUK, U, XK
      integer I, N
C     !DASH
      external HI, BYE
C
C               U(N), EMUK(N)
      dimension U(*), EMUK(*)
C
      call HI ('SARDINE')
C     !BEG
      do 100 I = 1,N
        EMUK(I) = exp(-U(I)*XK)
  100 continue
C     !END
      call BYE ('SARDINE')
C
      return
      end
