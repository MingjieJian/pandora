      subroutine WINE
     $(DMP1,DMP2,IND,N,WN)
C
C     Rudolf Loeser, 1980 Feb 01
C---- Dumps a WN-matrix used for frequency/angle sums.
C     !DASH
      save
C     !DASH
      real*8 WN
      integer IND, IPR01, IPR02, IQWDD, LUEO, N
      logical DMP1, DMP2
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
      equivalence (KZQ( 57),IPR01)
      equivalence (KZQ( 58),IPR02)
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
      equivalence (IQQ(146),IQWDD)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external ARROUT, HI, BYE
C
C               WN(N,N)
      dimension WN(*)
C
      call HI ('WINE')
C     !BEG
      if(DMP1.and.(.not.DMP2).and.(IQWDD.le.0)) then
        if((IND.ge.IPR01).and.(IND.le.IPR02)) then
          call ARROUT (LUEO, WN, N, N, 'Weight matrix WN')
        end if
      end if
C     !END
      call BYE ('WINE')
C
      return
      end
