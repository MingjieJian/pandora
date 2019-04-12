      subroutine TROCK
     $(N,XKL,SL,COP,CSF,XKT,ST,VEC)
C
C     Rudolf Loeser, 1995 Sep 07
C---- Computes total source function.
C     (This is version 2 of TROCK.)
C     !DASH
      save
C     !DASH
      real*8 COP, CSF, SL, ST, VEC, XKL, XKT
      integer N
C     !DASH
      external ARRADD, ARRDIV, ARRMUL, HI, BYE
C
C               XKL(N), SL(N), COP(N), CSF(N), XKT(N), ST(N), VEC(N)
      dimension XKL(*), SL(*), COP(*), CSF(*), XKT(*), ST(*), VEC(*)
C
      call HI ('TROCK')
C     !BEG
      call ARRMUL (XKL, SL,  ST,  N)
      call ARRMUL (COP, CSF, VEC, N)
      call ARRADD (ST,  VEC, ST,  N)
      call ARRDIV (ST,  XKT, ST,  N)
C     !END
      call BYE ('TROCK')
C
      return
      end
