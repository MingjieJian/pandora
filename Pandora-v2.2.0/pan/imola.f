      subroutine IMOLA
     $(IGRF,USE,PRNT,GRAF)
C
C     Rudolf Loeser, 1991 Jun 10
C---- Decides whether to print or plot an Iteration Summary.
C     !DASH
      save
C     !DASH
      integer IGRF, IQSMG, IQSUM, ISMSW
      logical GRAF, PRNT, USE
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
      equivalence (KZQ(119),ISMSW)
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
      equivalence (IQQ(283),IQSMG)
      equivalence (IQQ( 18),IQSUM)
C     !DASH
C     !EJECT
      external HI, BYE
C
      call HI ('IMOLA')
C     !BEG
      PRNT = .false.
      GRAF = .false.
      if(USE.and.(IQSUM.gt.0)) then
        if(ISMSW.gt.0) then
          PRNT = .true.
          GRAF = .true.
        else
          if(IQSMG.gt.0) then
            GRAF = .true.
          else
            PRNT = .true.
          end if
        end if
        if(IGRF.le.0) then
          GRAF = .false.
        end if
      end if
C     !END
      call BYE ('IMOLA')
C
      return
      end
