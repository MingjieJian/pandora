      subroutine DANK
     $(IBEG,IEND,OPAC,Z,F,TAU,LABEL,KODE,EDINT,EDTAU,IMG,W)
C
C     Rudolf Loeser, 1980 Sep 22
C---- Computes TAU as a function of Z, for DISMAL.
C     See also DREAR.
C     (This is version 4 of DANK.)
C     !DASH
      save
C     !DASH
      real*8 F, OPAC, TAU, W, Z
      integer IBEG, IEND, IMG, KNT, KODE
      logical EDINT, EDTAU
      character LABEL*(*)
C     !DASH
      external MOVE1, CATRIN, TUNA, HI, BYE
C
      dimension W(*)
C
C               OPAC(N), F(N), Z(N), TAU(N), IMG(N)
      dimension OPAC(*), F(*), Z(*), TAU(*), IMG(*)
C
      call HI ('DANK')
C     !BEG
      KNT = IEND-IBEG+1
      call MOVE1  (OPAC(IBEG), KNT, F(IBEG))
      call CATRIN (F(IBEG), KNT)
C
      call TUNA   (KNT, Z(IBEG), F(IBEG), TAU(IBEG), LABEL, KODE,
     $             EDINT, EDTAU, IMG, W)
C     !END
      call BYE ('DANK')
C
      return
      end
