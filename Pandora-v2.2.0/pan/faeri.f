      subroutine FAERI
     $(XCBL,ARRCO,CORC)
C
C     Rudolf Loeser, 2002 Oct 04
C---- Controls Composite Line Opacity analysis.
C     !DASH
      save
C     !DASH
      real*8 ARRCO, CORC, XCBL
      integer IQCOA, KCOAA, N, NCP, NO
      logical ENABL, GOOD
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
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(44),NCP)
C     !EJECT
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(185),KCOAA)
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
      equivalence (IQQ(320),IQCOA)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
C     !EJECT
      external PINANG, TAHAN, MELAKA, HI, BYE
C
C               XCBL(Miklen), ARRCO(NCP,N), CORC(NCP,N)
      dimension XCBL(*),      ARRCO(*),     CORC(*)
C
      call HI ('FAERI')
C     !BEG
      ENABL = (KOPAC(24).eq.1).or.(KOPAC(25).eq.1)
      if((IQCOA.gt.0).and.ENABL) then
C
        call PINANG     (NCP, N, ARRCO, CORC, XCBL, GOOD)
C
        if(GOOD) then
          if(KCOAA.gt.0) then
C----       Abbreviated format
            call TAHAN  (NO, NCP, N, CORC)
          else
C----       Regular format
            call MELAKA (NO, NCP, N, CORC)
          end if
        end if
C
      end if
C     !END
      call BYE ('FAERI')
C
      return
      end
