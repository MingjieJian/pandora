      subroutine RUE
     $(XCBL,W,RLINS,KLNS)
C
C     Rudolf Loeser, 1986 Feb 20
C---- Supervises calculation of Composite Lines cooling rate.
C     (This is version 3 of RUE.)
C     !DASH
      save
C     !DASH
      real*8 RLINS, W, XCBL
      integer IFREQ, IINTG, IN, IQKLC, IS, IVEC, IWAVE, KLNS, MOX, N,
     $        NAB
      logical DOIT
      character QNAME*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(45),NAB)
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
      equivalence (QZQ(  1),QNAME)
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
      equivalence (IQQ(194),IQKLC)
C     !DASH
C     !EJECT
      external KING, WORM, ZERO1, WGIVE, HI, BYE
C
      dimension W(*)
C
C               XCBL(Miklen), RLINS(N)
      dimension XCBL(*),      RLINS(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IFREQ ),(IN( 2),IWAVE ),(IN( 3),IINTG ),(IN( 4),IVEC  )
C
      call HI ('RUE')
C     !BEG
      DOIT = (QNAME.eq.'HYDROGEN').and.(IQKLC.gt.0).and.(NAB.eq.1)
C
      if(DOIT.and.(KOPAC(24).gt.0)) then
C       (Get, and allocate, W allotment)
        call KING  (IN, IS, MOX, 'RUE')
C
        call WORM  (RLINS, N, XCBL, W(IFREQ), W(IWAVE), W(IVEC),
     $              W(IINTG))
C
C       (Give back W allotment)
        call WGIVE (W, 'RUE')
C
        KLNS = 1
      else
        KLNS = 0
        call ZERO1 (RLINS, N)
      end if
C     !END
      call BYE ('RUE')
C
      return
      end
