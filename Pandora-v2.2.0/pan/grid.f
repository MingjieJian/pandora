      subroutine GRID
     $(XCBL,XLTIT,WAVE,DAMP,FMULT,BMULT,KRESN,KISLV,KOPAC,KOPAT,NOPAC)
C
C     Rudolf Loeser, 1995 Mar 02
C---- Initializes a Continuum Block.
C     (This is version 4 of GRID.)
C     !DASH
      save
C     !DASH
      real*8 BMULT, DAMP, FMULT, WAVE, XCBL, XLTIT
      integer KISLV, KKBULT, KKDAMP, KKISLV, KKKNTT, KKKONT, KKLAMD,
     $        KKLTIT, KKMULT, KKRESN, KOPAC, KOPAT, KRESN, NOPAC
C     !COM
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(54),KKLAMD)
      equivalence (KKK( 2),KKMULT)
      equivalence (KKK(39),KKBULT)
      equivalence (KKK( 1),KKLTIT)
      equivalence (KKK( 4),KKDAMP)
      equivalence (KKK( 3),KKKONT)
      equivalence (KKK(56),KKKNTT)
      equivalence (KKK(32),KKRESN)
      equivalence (KKK(33),KKISLV)
C     !DASH
      external ZERO1, CONTID, HI, BYE
C
C               XCBL(Miklen), KOPAC(Nopac), KOPAT(Nopac)
      dimension XCBL(*),      KOPAC(*),     KOPAT(*)
C
      call HI ('GRID')
C     !BEG
      call ZERO1  (XCBL, MIKLEN)
C
      call CONTID (KOPAC, 1, NOPAC, XCBL(KKKONT), 1, NOPAC)
      call CONTID (KOPAT, 1, NOPAC, XCBL(KKKNTT), 1, NOPAC)
C
      XCBL(KKLTIT) = XLTIT
      XCBL(KKLAMD) = WAVE
      XCBL(KKDAMP) = DAMP
      XCBL(KKMULT) = FMULT
      XCBL(KKBULT) = BMULT
      XCBL(KKRESN) = KRESN
      XCBL(KKISLV) = KISLV
C     !END
      call BYE ('GRID')
C
      return
      end
