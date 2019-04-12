      subroutine BURMA
     $(X,W,KIPE,LEGEND,XCBL,WTAB,INDX,MF,ML,IMG,WTABW,INDXW,IPER)
C
C     Rudolf Loeser, 1973 May 18
C---- Drives production of Contributors' Summary.
C     !DASH
      save
C     !DASH
      real*8 W, WTAB, WTABW, X, XCBL
      integer ICNTRB, IMG, IN, INDX, INDXW, IOPAC, IPER, IS, ITAU, IWVL,
     $        KIPE, LEGEND, MF, ML, MOX, NW
C     !COM
C---- OPACITY     as of 2007 Jan 12
C     Paraphernalia for background absorption/emission contributors.
C
C     (Must recompile BARE, BRACE, FORAGER & SHARI when changing NABS!)
      parameter   (NABS=45)
C
      integer     NABS,NOPAC,KOPAC,TOPAC,LOPAC
      character   CNAME*24,SYMID*1,SHNAM*6
      dimension   KOPAC(NABS),LOPAC(NABS),SYMID(NABS)
      dimension   CNAME(NABS),SHNAM(NABS)
C
      common      /OPAC1/ NOPAC
      common      /OPAC2/ KOPAC
      common      /OPAC3/ LOPAC
      common      /OPAC4/ CNAME
      common      /OPAC5/ SYMID
      common      /OPAC6/ SHNAM
C
C     NOPAC = number of contributors
C     KOPAC = contributor status switch: 0 = don't use, 1 = use
C     CNAME = name (description) of contributor
C             NOTE: This sequence of names establishes the indices or
C                   ordinals by which the contributors are also known.
C     SHNAM = "short" name of contributor
C     LOPAC = "printout order" list of contributor ordinals
C     SYMID = scratch space for symbolic identifiers
C     .
C     !DASH
C     !EJECT
      external MALDIVE, JENNY, BENJAMN, BRACE, TONY, WGIVE, HI, BYE
C
      dimension X(*), W(*)
C
C               XCBL(Miklen), INDX(Numkon), IMG(N), IPER(Nopac,Numkon),
      dimension XCBL(*),      INDX(*),      IMG(*), IPER(*),
C
C               WTABW(Numkon), INDXW(Numkon), WTAB(Numkon)
     $          WTABW(*),      INDXW(*),      WTAB(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IOPAC ),(IN( 2),IWVL  ),(IN( 3),ITAU  ),(IN( 4),ICNTRB)
C
      call HI ('BURMA')
C     !BEG
C     (Get, and allocate, W allotment)
      call MALDIVE (IN, IS, MOX, 'BURMA')
C
C---- Glean data
      call JENNY   (X, W, XCBL, KIPE, W(IOPAC), W(ITAU), W(ICNTRB),
     $              IMG, WTAB, INDX, NOPAC, IPER, MF, ML, WTABW,
     $              INDXW, NW)
C---- Set up plot codes
      call BRACE   (IPER, NW)
C---- Print
      call BENJAMN (NW, WTABW, INDXW, IPER, KIPE, NOPAC, LEGEND)
C---- Plot
      call TONY    (NW, WTABW, INDXW, IPER, KIPE, NOPAC, W(IWVL))
C
C     (Give back W allotment)
      call WGIVE   (W, 'BURMA')
C     !END
      call BYE ('BURMA')
C
      return
      end
