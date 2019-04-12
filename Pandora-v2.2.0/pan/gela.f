      subroutine GELA
     $(XJNU,N,LAG)
C
C     Rudolf Loeser, 1982 Jan 29
C---- Checks XJNU, and sets validity flag.
C
C     LAG will be set =0 if no element of XJNU is <0;
C     LAG will be set >0 otherwise.
C
C     !DASH
      save
C     !DASH
      real*8 XJNU
      integer LAG, N
C     !DASH
      external MINUSD, HI, BYE
C
C               XJNU(N)
      dimension XJNU(*)
C
      call HI ('GELA')
C     !BEG
      call MINUSD (XJNU,1,N,LAG)
C     !END
      call BYE ('GELA')
C
      return
      end
