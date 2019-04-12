      subroutine BOLO
     $(NO,ADS,ADMAS,R1N,SPHERE,LFB,LINK,IJECT)
C
C     Rudolf Loeser, 1986 Feb 21
C---- Writes heading for Continuum Flux printout.
C     !DASH
      save
C     !DASH
      real*8 ADMAS, ADS, R1N
      integer IJECT, IQENH, LFB, LINK, NO
      logical SPHERE
      character BLANK*1, TAT*6, TIT*4
C     !COM
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
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
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
      equivalence (IQQ( 38),IQENH)
C     !DASH
C     !EJECT
      external SAXON, LINER, HI, BYE
C
      call HI ('BOLO')
C     !BEG
      if(NO.gt.0) then
        call SAXON     (NO, LFB, LINK, IJECT)
C
        if(SPHERE) then
          write (NO,100)
  100     format(T13,'Calculated using spherical coordinates.'/
     $           T13,'The total flux is the sum of contributions ',
     $               'from the Disk and from the Shell.')
          TIT = 'SHLR'
        else
          write (NO,101)
  101     format(T13,'Calculated using plane-parallel coordinates.')
          if(IQENH.gt.0) then
            call LINER (1, NO)
            write (NO,102)
  102       format(T13,'R-squared Source Function enhancement ',
     $                 'included.')
          end if
          TIT = BLANK
        end if
        call LINER     (2, NO)
        write (NO,103) ADS,ADMAS,R1N
  103   format(T13,'Star/Sun angular diameter ratio, ADS =',1PE13.6/
     $         T13,'Angular diameter, ADMAS =',E13.6,' milliarcseconds'/
     $         T13,'Radius of curvature, R1N =',E13.6,' km')
        call LINER     (2, NO)
        if(WAVENO) then
          TAT = '  WN  '
          write (NO,104)
  104     format(T13,'WN     = Wavenumber, in /cm (for wavelength, ',
     $                         'turn option WAVENUMB = OFF)')
        else
          TAT = '  WL  '
          write (NO,105)
  105     format(T13,'WL     = Wavelength, in Angstroms (for ',
     $                         'wavenumber, turn option WAVENUMB = ON)')
        end if
C     !EJECT
        write (NO,106)
  106   format(T13,'WVL    = Wavelength, in some form of meter, ',
     $                       'as indicated'/
     $         T13,'OM     = Opacity Multiplier (if other than 1)'/
     $         T13,'F/Hz   = Flux 4*Pi*H at R(Star), ',
     $                       'in ergs/cm**2/s/Hz'/
     $         T13,'F/A    = Flux 4*Pi*H at R(Star), ',
     $                       'in ergs/cm**2/s/Angstrom'/
     $         T13,'TB     = Flux Brightness Temperature, in Kelvins')
        if(SPHERE) then
          write (NO,107)
  107     format(T13,'SHLR   = (Shell Flux)/(Total Flux)')
        end if
        call LINER (3, NO)
        write (NO,108) TAT,TIT
  108   format(' ',12('----------'),'-------'//
     $         ' ',12X,A6,7X,'WVL',11X,'OM',8X,'F/Hz',8X,'F/A',
     $             10X,'TB',7X,A4)
        call LINER (1, NO)
      end if
C     !END
      call BYE ('BOLO')
C
      return
      end
