      subroutine PLINTH
     $(WVL,XCBL,ISWA,LU)
C
C     Rudolf Loeser, 1983 Nov 03
C---- Prints details of Continuum Opacity at WVL Angstroms.
C     !DASH
      save
C     !DASH
      real*8 WVL, XCBL, dummy
      integer ISWA, KKBULT, KKCAPP, KKCO, KKISWA, KKMULT, KKOPAC,
     $        KKSIGS, KNFRM, LU, N, jummy
      logical LINE, ZALBD
      character BLANK*1, PLTID*1, TEXT*12, TPOP*3
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 26),KNFRM)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(19),KKCO  )
      equivalence (KKK( 6),KKOPAC)
      equivalence (KKK( 2),KKMULT)
      equivalence (KKK(43),KKISWA)
      equivalence (KKK(39),KKBULT)
      equivalence (KKK(29),KKCAPP)
      equivalence (KKK(45),KKSIGS)
C     !EJECT
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
      external MARABOU, ABJECT, KRAAL, CONTDI, POOBLE, SETC, HI, BYE
C
C               XCBL(Miklen), ISWA(Nopac)
      dimension XCBL(*),      ISWA(*)
C
      dimension PLTID(4)
C
      data ZALBD,LINE /.true., .false./
C
      call HI ('PLINTH')
C     !BEG
      if(LU.gt.0) then
        call KRAAL   (XCBL(KKCO), XCBL(KKOPAC), XCBL(KKMULT), N, NOPAC,
     $                KNFRM)
        call CONTDI  (XCBL(KKISWA), 1, NOPAC, ISWA, 1, NOPAC)
        call SETC    (SYMID, 1, NOPAC, BLANK)
        call SETC    (PLTID, 1, 4    , BLANK)
        call MARABOU (WVL, dummy, TEXT)
C
        call ABJECT  (LU)
        call POOBLE  (N, NOPAC, TEXT, XCBL(KKOPAC), XCBL(KKMULT),
     $                XCBL(KKBULT), XCBL(KKCAPP), XCBL(KKSIGS), dummy,
     $                ZALBD, XCBL(KKCO), KNFRM, LU, 0, ISWA, PLTID,
     $                dummy, dummy, 0, TPOP, jummy, LINE)
      end if
C     !END
      call BYE ('PLINTH')
C
      return
      end
