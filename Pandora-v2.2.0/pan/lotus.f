      subroutine LOTUS
     $(N,Z,F,KZERO,FREF)
C
C     Rudolf Loeser, 1980 Nov 03
C---- Computes F(Z=0), for HSE.
C     (This is version 5 of LOTUS.)
C     !DASH
      save
C     !DASH
      real*8 F, FREF, ONE, XDEN, XNUM, Z
      integer JZERO, KZERO, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, HI, BYE
C
C               Z(N), F(N)
      dimension Z(*), F(*)
C
      call HI ('LOTUS')
C     !BEG
      JZERO = -KZERO
C
      if((KZERO.ge.1).and.(KZERO.le.N)) then
        FREF = F(KZERO)
C
      else if((JZERO.ge.1).and.(JZERO.lt.N)) then
        XNUM = F(JZERO)*Z(JZERO+1)-F(JZERO+1)*Z(JZERO)
        XDEN = Z(JZERO+1)-Z(JZERO)
        call DIVIDE (XNUM,XDEN,FREF)
C
      else
        FREF = ONE
      end if
C     !END
      call BYE ('LOTUS')
C
      return
      end
