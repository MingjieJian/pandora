      subroutine SHARI
     $(XCBL,Z,TE)
C
C     Rudolf Loeser, 1995 Apr 24
C---- Produces a full continuum wavelengths summary.
C     (This is version 3 of SHARI.)
C     !DASH
      save
C     !DASH
      real*8 CONSW, ST1, TE, TMAX, XCBL, YT1, Z, ZT1
      integer I, IHSE, ILYM, IOVR, IQWSP, ISIG, ISLV, ITS, IWSMD,
     $        KKACTO, KKBULT, KKCO, KKDAMP, KKISLV, KKITS, KKJNU,
     $        KKKNTT, KKKODE, KKKONT, KKKTIT, KKLPRD, KKLTIT, KKMULT,
     $        KKRESN, KKSCON, KKTAUK, KKTSCN, KKTTAU, KTRU, LINES, LPRD,
     $        MXCHR, N, NO, NRES
      character KONSWI*82
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
      equivalence (KZQ(151),IWSMD)
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(216),IQWSP)
C     !EJECT
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(12),KKTAUK)
      equivalence (KKK(14),KKSCON)
      equivalence (KKK(13),KKJNU )
      equivalence (KKK( 1),KKLTIT)
      equivalence (KKK( 4),KKDAMP)
      equivalence (KKK( 2),KKMULT)
      equivalence (KKK(42),KKITS )
      equivalence (KKK(18),KKACTO)
      equivalence (KKK(34),KKLPRD)
      equivalence (KKK(32),KKRESN)
      equivalence (KKK(33),KKISLV)
      equivalence (KKK(40),KKKTIT)
      equivalence (KKK(39),KKBULT)
      equivalence (KKK(19),KKCO  )
      equivalence (KKK( 3),KKKONT)
      equivalence (KKK(48),KKKODE)
      equivalence (KKK(56),KKKNTT)
      equivalence (KKK(49),KKTTAU)
      equivalence (KKK(50),KKTSCN)
C
C---- TABLET      as of 2007 Feb 21
      integer     KONLIM,KONWAL
      parameter   (KONLIM=20000)
      real*8      CONTIT,CONWAV
      integer     NUMKON,NUMTRU,NMKUSE,KONADR,KONTYP,KONLIC,KONNSH
      dimension   CONTIT(KONLIM),CONWAV(KONLIM),KONADR(KONLIM),
     $            KONTYP(KONLIM),KONLIC(KONLIM),KONNSH(KONLIM)
      common      /TABLET0/ KONWAL,NUMKON,NUMTRU,NMKUSE
      common      /TABLET1/ CONWAV
      common      /TABLET2/ CONTIT
      common      /TABLET3/ KONADR
      common      /TABLET4/ KONTYP
      common      /TABLET5/ KONLIC
      common      /TABLET6/ KONNSH
C
C     Index, and other data, for Continuum Data Blocks.
C
C     KONWAL - (= KONLIM)
C     NUMKON - total number of Blocks
C     NUMTRU - number of line-specific Blocks ( .le. NUMKON)
C     NMKUSE - number of Blocks to be used for SIAM scratch storage
C
C     CONTIT - Block name (also called "Header Code" or XLTIT,SLTIT)
C     CONWAV - wavelength (Angstroms)
C     KONADR - file address of Block
C     KONTYP - Block code (labelled-common "kwack" via subroutine BEECH)
C     KONNSH - number of supplementary headers (shared blocks only)
C     KONLIC - line transition descriptor, = 100*iu+il (if needed)
C     .
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C
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
      external CALTROP, LEYTE, KLUNK, BLURT, BREAKER, MASSAGE, CREAKER,
     $         DEREK, SURF, SWELL, MOVE1, LETHE, BOSSY, HI, BYE
C
C               Z(N), TE(N), XCBL(Miklen)
      dimension Z(*), TE(*), XCBL(*)
C
      dimension CONSW(NABS)
C
      data MXCHR /82/
C
      call HI ('SHARI')
C     !BEG
C
C---- Part 0 (always)
C
      call SURF    (NO, 0)
      do 100 I = 1,NUMKON
        call LEYTE (XCBL, MIKLEN, KONADR(I))
        call BOSSY (XCBL(KKLTIT), KONNSH(I), XCBL(KKKTIT))
  100 continue
      call DEREK
C     !EJECT
      if(IQWSP.gt.0) then
C
C----   Part 1
C
        call SURF      (NO, 1)
        call BREAKER   (NO)
        LINES = 55
C
        do 101 I = 1,NUMKON
          call LEYTE   (XCBL, MIKLEN, KONADR(I))
          ITS  = XCBL(KKITS)
          KTRU = XCBL(KKKODE)
          call KLUNK   (Z, N, ZT1, XCBL(KKSCON), ST1, XCBL(KKJNU),
     $                  YT1, XCBL(KKTAUK), TMAX)
          call BLURT   (I, LINES, NO, CONWAV(I), XCBL(KKDAMP),
     $                  XCBL(KKMULT), ZT1, ST1, YT1, ITS, TMAX,
     $                  XCBL(KKLTIT), KONNSH(I), XCBL(KKKTIT),
     $                  XCBL(KKBULT), KONTYP(I))
          if(KTRU.eq.1) then
            call KLUNK (Z, N, ZT1, XCBL(KKTSCN), ST1, XCBL(KKJNU),
     $                  YT1, XCBL(KKTTAU), TMAX)
            call BLURT (-1, LINES, NO, CONWAV(I), XCBL(KKDAMP),
     $                  XCBL(KKMULT), ZT1, ST1, YT1, ITS, TMAX,
     $                  XCBL(KKLTIT), KONNSH(I), XCBL(KKKTIT),
     $                  XCBL(KKBULT), KONTYP(I))
          end if
  101   continue
C     !EJECT
        if(IWSMD.eq.1) then
C
C----     Part 2
C
          call SURF        (NO, 2)
          call CREAKER     (NO)
          LINES = 55
C
          do 102 I = 1,NUMKON
            call LEYTE     (XCBL, MIKLEN, KONADR(I))
            ISIG = XCBL(KKACTO)
            LPRD = XCBL(KKLPRD)
            NRES = XCBL(KKRESN)
            ISLV = XCBL(KKISLV)
            KTRU = XCBL(KKKODE)
            call MASSAGE   (2, ISIG, IOVR, IHSE, ILYM)
            call MOVE1     (XCBL(KKKONT), NOPAC, CONSW)
            call LETHE     (XCBL(KKCO), NOPAC, N, CONSW)
            call CALTROP   (CONSW, NOPAC, KONSWI, MXCHR)
            call SWELL     (I, LINES, NO, CONWAV(I), IOVR, IHSE, ILYM,
     $                      LPRD, KONLIC(I), NRES, ISLV, KONSWI)
            if(KTRU.eq.1) then
              call MOVE1   (XCBL(KKKNTT), NOPAC, CONSW)
              call LETHE   (XCBL(KKCO), NOPAC, N, CONSW)
              call CALTROP (CONSW, NOPAC, KONSWI, MXCHR)
              call SWELL   (-1, LINES, NO, CONWAV(I), IOVR, IHSE, ILYM,
     $                      LPRD, KONLIC(I), NRES, ISLV, KONSWI)
            end if
  102     continue
        end if
      end if
C     !END
      call BYE ('SHARI')
C
      return
      end
