      subroutine SVERRI
     $(LU)
C
C     Rudolf Loeser, 2003 May 06
C---- Prints a legend on intensity printouts.
C     (This is version 2 of SVERRI.)
C     !DASH
      save
C     !DASH
      real*8 TML, TSM
      integer IQAPP, IQUTM, LU
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
      equivalence (RZQ( 21),TSM  )
      equivalence (RZQ( 73),TML  )
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
      equivalence (IQQ(261),IQAPP)
      equivalence (IQQ(321),IQUTM)
C     !DASH
      external LINER, HI, BYE
C     !EJECT
C
      call HI ('SVERRI')
C     !BEG
      if((LU.gt.0).and.(IQAPP.le.0)) then
        call LINER (1,LU)
C
        write (LU,100) TSM,TML
  100   format(' ','*NOTE - Summing the intensity integrals is ',
     $             'controlled by option USETSM and input parameters ',
     $             'TSM =',1PE8.1,' and TML =',E8.1,'.')
        if(IQUTM.gt.0) then
          write (LU,101)
  101     format(' ','        Contributions from the regions 0 < ',
     $               'TMU < TSM and beyond the second TMU > TML ',
     $               'have been neglected.')
        else
          write (LU,102)
  102     format(' ','        Since USETSM = off, only contributions ',
     $               'from the region beyond the second TMU > TML ',
     $               'have been neglected.')
        end if
C
      end if
C     !END
      call BYE ('SVERRI')
C
      return
      end
