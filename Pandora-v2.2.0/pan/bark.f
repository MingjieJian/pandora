      subroutine BARK
C
C     Rudolf Loeser, 1988 Jun 06
C---- Converts option settings to "equivalent switch" settings.
C     (This is version 2 of BARK.)
C     !DASH
      save
C     !DASH
      integer IQBNC, IQDIR, IQISO, IQJIN, IQNVT, IQPDV, IQPPS, IQRCO,
     $        IQSCP, IRPUN, IRUNT, ISCRS, ITOPE, IXSTA, JBDNC, JSTIN,
     $        NOION, NVOIT
C     !COM
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
      equivalence (IQQ(232),IQISO)
      equivalence (IQQ(233),IQDIR)
      equivalence (IQQ(235),IQNVT)
      equivalence (IQQ(236),IQPPS)
      equivalence (IQQ(237),IQBNC)
      equivalence (IQQ(240),IQJIN)
      equivalence (IQQ(241),IQRCO)
      equivalence (IQQ(243),IQSCP)
      equivalence (IQQ(242),IQPDV)
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
      equivalence (KZQ( 88),ISCRS)
      equivalence (KZQ( 94),NOION)
      equivalence (KZQ( 39),NVOIT)
      equivalence (KZQ( 11),IXSTA)
      equivalence (KZQ( 40),JBDNC)
      equivalence (KZQ( 17),ITOPE)
      equivalence (KZQ(  7),JSTIN)
      equivalence (KZQ( 63),IRPUN)
      equivalence (KZQ( 93),IRUNT)
C     !DASH
      external HI, BYE
C
      call HI ('BARK')
C     !BEG
      if(IQISO.eq.1) then
        ISCRS = 0
      else
        ISCRS = 1
      end if
C
      if(IQDIR.eq.1) then
        NOION = 0
      else
        NOION = 1
      end if
C
      if(IQNVT.eq.1) then
        NVOIT = 1
      else
        NVOIT = 0
      end if
C
      if(IQPPS.eq.1) then
        IXSTA = 1
      else
        IXSTA = 0
      end if
C     !EJECT
      if(IQBNC.eq.1) then
        JBDNC = 1
      else
        JBDNC = 0
      end if
C
      if(IQJIN.eq.1) then
        JSTIN = 1
      else
        JSTIN = 0
      end if
C
      if(IQRCO.eq.1) then
        IRPUN = 1
      else
        IRPUN = 0
      end if
C
      if(IQPDV.eq.1) then
        IRUNT = 1
      else
        IRUNT = 0
      end if
C
      if(IQSCP.eq.1) then
        ITOPE = 1
      else
        ITOPE = 0
      end if
C     !END
      call BYE ('BARK')
C
      return
      end
