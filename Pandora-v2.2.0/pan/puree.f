      subroutine PUREE
     $(U,N,XK,KK,EMUK)
C
C     Rudolf Loeser, 2006 Oct 31
C---- Computes EMUK, for the Lyman calculations.
C     (This is version 3 of PUREE.)
C     !DASH
      save
C     !DASH
      real*8 EMUK, U, XK
      integer J, KK, N
C     !DASH
      external SARDINE, HI, BYE
C
C               U(N), XK(KKX), EMUK(N,KKX)
      dimension U(*), XK(*),   EMUK(N,*)
C
      call HI ('PUREE')
C     !BEG
      do 100 J = 1,KK
        call SARDINE (XK(J), N, U, EMUK(1,J))
  100 continue
C     !END
      call BYE ('PUREE')
C
      return
      end
