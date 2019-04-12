      subroutine CAROOM
     $(DOSN1)
C
C     Rudolf Loeser, 2003 Jul 02
C---- Determines whether to do the "Special N1" calculation.
C     !DASH
      save
C     !DASH
      integer IOMX, IOVER, IQAN1, LUEO, NDSN1
      logical DOSN1
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(198),NDSN1)
      equivalence (KZQ(  8),IOMX )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
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
      equivalence (IQQ(272),IQAN1)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, MASHED, HI, BYE
C
      call HI ('CAROOM')
C     !BEG
      DOSN1 = IQAN1.gt.0
C
      if(DOSN1) then
        if(IOVER.eq.1) then
          DOSN1 = NDSN1.ne.1
        end if
C
        if((IOMX.eq.1).and.(.not.DOSN1)) then
          call MESHED ('CAROOM', 3)
          write (LUEO,100)
  100     format(' ','Note: the Special-N1 Calculation has been ',
     $               'suppressed --- input parameter NDSN1.')
          call MASHED ('CAROOM')
        end if
      end if
C     !END
      call BYE ('CAROOM')
C
      return
      end
