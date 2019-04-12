      subroutine DELLA
     $(N,S,BHS,XA,YA,XJNU,LAG)
C
C     Rudolf Loeser, 1981 Jul 22
C---- Computes Continuum Source Function, for TUAREG.
C     !DASH
      save
C     !DASH
      real*8 BHS, S, XA, XJNU, YA
      integer LAG, N
C     !DASH
      external ARRMUL, ARRADD, MOVE1, HI, BYE
C
C               S(N), XA(N), YA(N), XJNU(N), BHS(N)
      dimension S(*), XA(*), YA(*), XJNU(*), BHS(*)
C
      call HI ('DELLA')
C     !BEG
      if(LAG.le.0) then
        call ARRMUL (XA,XJNU,S,N)
        call ARRADD (S,YA,S,N)
      else
        call MOVE1  (BHS,N,S)
      end if
C     !END
      call BYE ('DELLA')
C
      return
      end
