      subroutine LEMUR
     $(N,CO,XJNU,VEC,YNT)
C
C     Rudolf Loeser, 1986 Mar 07
C---- Computes integrand, for PASANG.
C     !DASH
      save
C     !DASH
      real*8 CO, FAC, VEC, XJNU, YNT
      integer N
C     !DASH
      external MOVE1, NEGATE, CONMUL, RIGEL, LOGOS, HI, BYE
C
C               CO(Nopac,N), XJNU(N), YNT(N), VEC(N)
      dimension CO(*),       XJNU(*), YNT(*), VEC(*)
C
      call HI ('LEMUR')
C     !BEG
      call MOVE1  (XJNU,N,VEC)
      call NEGATE (VEC,N)
      call RIGEL  (53,FAC)
      call CONMUL (FAC,VEC,N)
      call LOGOS  (N,26,CO,VEC,YNT)
C     !END
      call BYE ('LEMUR')
C
      return
      end
