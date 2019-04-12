      subroutine BRACE
     $(IPER,NW)
C
C     Rudolf Loeser, 1988 Jun 08
C---- Sets up plot codes, for contributors summary plots.
C     (This is version 3 of BRACE.)
C     !DASH
      save
C     !DASH
      integer I, IMUX, IPER, L, LPNT, MUX, NW, jummy
      character BLANK*1
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
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
C     !EJECT
      external  MINMAXI, SORTI, SETC, HI, BYE
      intrinsic max, min
C
C               IPER(Nopac,Numkon)
      dimension IPER(NOPAC,*)
C
      dimension MUX(NABS), LPNT(NABS)
C
      call HI ('BRACE')
C     !BEG
      do 100 I = 1,NOPAC
        call MINMAXI (IPER(I,1), NOPAC, NW, jummy, IMUX)
        MUX(I) = -IPER(I,IMUX)
  100 continue
C
      call SORTI     (MUX, NOPAC, LPNT, 'MUX')
C
      call SETC      (SYMID, 1, NOPAC, BLANK)
      do 101 I = 1,(min(NOPAC,26))
        L = LPNT(I)
        SYMID(L) = ALPHS(I)
  101 continue
C     !END
      call BYE ('BRACE')
C
      return
      end
