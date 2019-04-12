      subroutine COULOMB
     $(ION1,ION2,TE,XNE,XQ)
C
C     Rudolf Loeser, 1981 Jan 30
C---- Computes Coulomb cross-section
C     for the interaction "ION1 <-> ION2".
C     ION1, ION2 each may take on the values:
C     'E   ', 'H+  ', 'HE+ ', 'HE++', 'H   ', 'HE  '.
C     (However, cross-sections for some of all possible interactions
C     are not computable, - see listing of 'KRAIT'.)
C     !DASH
      save
C     !DASH
      real*8 P, TE, XNE, XQ
      integer KODE
      character ION1*4, ION2*4
C     !DASH
      external KRAIT, ELPH, PYTHON, COBRA, HI, BYE
C
      call HI ('COULOMB')
C     !BEG
      call KRAIT    (ION1, ION2, KODE)
      if(KODE.le.4) then
        call ELPH   (XNE , TE, P)
        call PYTHON (KODE, TE, P, XQ)
      else
        KODE = KODE-4
        call COBRA  (KODE, TE,    XQ)
      end if
C     !END
      call BYE ('COULOMB')
C
      return
      end
