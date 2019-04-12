      subroutine FUTILE
     $(N,KK,DUMP,CALLER)
C
C     Rudolf Loeser, 2004 Mar 08
C---- Sets up dumps, for TEUFEL.
C     !DASH
      save
C     !DASH
      integer IQLYD, KK, KLDIN, KLFIN, LUEO, N
      logical DOK, DUMP, FOK
      character CALLER*(*)
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
      equivalence (KZQ(219),KLDIN)
      equivalence (KZQ(220),KLFIN)
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
      equivalence (IQQ(116),IQLYD)
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external MESHED, HI, BYE
C
      call HI ('FUTILE')
C     !BEG
      DOK  = (KLDIN.gt.0).and.(KLDIN.lt.N)
      FOK  = (KLFIN.gt.0).and.(KLFIN.lt.KK)
      DUMP = (IQLYD.gt.0).and.DOK.and.FOK
C
      if(DUMP) then
        call MESHED (CALLER, 2)
        write (LUEO, 100)
  100   format(' ','Dump of Lyman continuum data.')
      end if
C     !END
      call BYE ('FUTILE')
C
      return
      end
