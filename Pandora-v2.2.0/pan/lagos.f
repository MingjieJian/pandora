      subroutine LAGOS
     $(N,CO,XJNU,B,VEC,YNT)
C
C     Rudolf Loeser, 1986 Feb 20
C---- Computes integrand, for PARSLEY.
C     (This is version 2 of LAGOS.)
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
      call HI ('LAGOS')
C     !BEG
      call ARRSUB (B,XJNU,VEC,N)
      call RIGEL  (53,FAC)
      call CONMUL (FAC,VEC,N)
      call LOGOS  (N,24,CO,VEC,YNT)
C     !END
      call BYE ('LAGOS')
C
      return
      end
