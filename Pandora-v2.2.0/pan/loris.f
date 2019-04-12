      subroutine LORIS
     $(NO,LFB,LINK,IJECT)
C
C     Rudolf Loeser, 1986 Feb 21
C---- Prints a heading, for CARINA.
C     !DASH
      save
C     !DASH
      integer IJECT, IQENH, IQIFF, LFB, LINK, NO
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
      equivalence (IQQ(141),IQIFF)
      equivalence (IQQ( 38),IQENH)
C
C---- MOSTAR      as of 2000 Sep 26
      real*8      COREWL,COREWN
      integer     ICORE
      character   WLAB1*10,WLAB2*2,WLAB3*12,WLAB4*10,WLAB5*2
      logical     WAVENO,SLINE,WHOLE,RED,BLUE
      common      /MOSTAR1/ COREWL,COREWN
      common      /MOSTAR2/ ICORE
      common      /MOSTAR3/ WLAB1,WLAB2,WLAB3,WLAB4,WLAB5
      common      /MOSTAR4/ WAVENO,SLINE,WHOLE,RED,BLUE
C     Wavelength/Wavenumber print/plot controls (subroutine WUMBLE).
C     .
C     !DASH
C     !EJECT
      external NAXOS, LINER, HI, BYE
C
      call HI ('LORIS')
C     !BEG
      if(NO.gt.0) then
        call NAXOS   (NO, LFB, LINK, IJECT)
C
        write (NO,100)
  100   format(T13,'Calculated using plane-parallel coordinates.')
        if(IQENH.gt.0) then
          call LINER (1, NO)
          write (NO,101)
  101     format(T13,'R-squared Source function enhancement included.')
        end if
        if(IQIFF.gt.0) then
          call LINER (1, NO)
          write (NO,102)
  102     format(T13,'Front-face incident radiation affects the ',
     $               'source function.')
        end if
        call LINER   (2, NO)
        if(WAVENO) then
          write (NO,103)
  103     format(T13,'WN   = Wavenumber, in /cm (for wavelength, ',
     $                         'turn option WAVENUMB = OFF)')
        else
          write (NO,104)
  104     format(T13,'WL   = Wavelength, in Angstroms (for wave',
     $                         'number, turn option WAVENUMB = ON)')
        end if
        write (NO,105)
  105   format(T13,'WVL  = Wavelength, in some form of meter, ',
     $                     'as indicated'/
     $         T13,'OM   = Opacity Multiplier (if other than 1)'/
     $         T13,'I/Hz = absolute Intensity ',
     $                     'in ergs/cm**2/s/sr/Hz'/
     $         T13,'I/A  = absolute Intensity ',
     $                     'in ergs/cm**2/s/sr/Angstrom'/
     $         T13,'TB   = Brightness Temperature, in Kelvins')
      end if
C     !END
      call BYE ('LORIS')
C
      return
      end
