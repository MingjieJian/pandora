      subroutine SENF
     $(DUMP,CALLER,I,LU)
C
C     Rudolf Loeser, 1987 Sep 01
C---- Sets up LU for an ADAIR dump, for SKUA.
C     (This is version 2 of SENF.)
C     !DASH
      save
C     !DASH
      integer I, IQSOI, ISOD, LU, LUEO
      logical DUMP
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
      equivalence (KZQ( 97),ISOD )
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
      equivalence (IQQ(218),IQSOI)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external MESHED, HI, BYE
C
      call HI ('SENF')
C     !BEG
      LU = 0
C
      if(DUMP) then
        if((IQSOI.gt.0).and.(I.eq.ISOD)) then
          LU = LUEO
          call MESHED (CALLER, 2)
        end if
      end if
C     !END
      call BYE ('SENF')
C
      return
      end
