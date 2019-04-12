      subroutine LALLER
     $(N,CO,XJNU,B,VEC,YNT)
C
C     Rudolf Loeser, 1987 Nov 18
C---- Computes integrands, for CO-lines cooling rate.
C     !DASH
      save
C     !DASH
      real*8 B, CO, FAC, VEC, XJNU, YNT
      integer N
C     !DASH
      external ARRSUB, CONMUL, RIGEL, LOGOS, HI, BYE
C
C               CO(Nopac,N), XJNU(N), B(N), YNT(N), VEC(N)
      dimension CO(*),       XJNU(*), B(*), YNT(*), VEC(*)
C
      call HI ('LALLER')
C     !BEG
      call ARRSUB (B,XJNU,VEC,N)
      call RIGEL  (53,FAC)
      call CONMUL (FAC,VEC,N)
      call LOGOS  (N,27,CO,VEC,YNT)
C     !END
      call BYE ('LALLER')
C
      return
      end
