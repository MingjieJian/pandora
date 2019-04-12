      subroutine DREAR
     $(IBEG,IEND,OPAC,PREF,TAUKIN,F,TAU,LABEL,KODE,EDINT,EDTAU,IMG,W)
C
C     Rudolf Loeser, 1980 Sep 22
C---- Computes TAU as a function of TAUKIN, for DISMAL.
C     See also DANK.
C     (This is version 2 of DREAR.)
C     !DASH
      save
C     !DASH
      real*8 F, OPAC, PREF, TAU, TAUKIN, W
      integer IBEG, IEND, IMG, KNT, KODE
      logical EDINT, EDTAU
      character LABEL*(*)
C     !DASH
      external TUNA, ARRDIV, HI, BYE
C
      dimension W(*)
C
C               OPAC(N), PREF(N), F(N), TAUKIN(N), TAU(N), IMG(N)
      dimension OPAC(*), PREF(*), F(*), TAUKIN(*), TAU(*), IMG(*)
C
      call HI ('DREAR')
C     !BEG
      KNT = IEND-IBEG+1
      call ARRDIV (OPAC(IBEG), PREF(IBEG), F(IBEG), KNT)
C
      call TUNA   (KNT, TAUKIN(IBEG), F(IBEG), TAU(IBEG), LABEL, KODE,
     $             EDINT, EDTAU, IMG, W)
C     !END
      call BYE ('DREAR')
C
      return
      end
