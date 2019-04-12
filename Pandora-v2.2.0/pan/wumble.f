      subroutine WUMBLE
     $(NW,WVLEN,WVNUM,WTAB,CORE,LINE)
C
C     Rudolf Loeser, 2000 Sep 15
C---- Sets up data in MOSTAR for printing and plotting of results from
C     spectrum calculations
C     !DASH
      save
C     !DASH
      real*8 CORE, WTAB, WVLEN, WVNUM, ZERO
      integer IQWNM, LINE, NW
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
      equivalence (IQQ(290),IQWNM)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external MOVE1, WANDA, QUEBEC, HI, BYE
C
C               WVLEN(NW), WVNUM(NW), WTAB(NW)
      dimension WVLEN(*),  WVNUM(*),  WTAB(*)
C
      call HI ('WUMBLE')
C     !BEG
      WAVENO = IQWNM.gt.0
C
      if(WAVENO) then
        WLAB1 = 'wavenumber'
        WLAB2 = 'WN'
        WLAB3 = ' Wavenumber '
        WLAB4 = '   (/cm)  '
        WLAB5 = 'WN'
        call MOVE1  (WVNUM, NW, WTAB)
      else
        WLAB1 = 'wavelength'
        WLAB2 = 'WL'
        WLAB3 = 'Delta-Lambda'
        WLAB4 = '(Angstrom)'
        WLAB5 = 'DL'
        call MOVE1  (WVLEN, NW, WTAB)
      end if
C
      SLINE = LINE.gt.0
C
      if(SLINE) then
        COREWL = CORE
        call WANDA  (COREWL, COREWN)
        call QUEBEC (WVLEN, NW, WLAB5, 'WUMBLE', ICORE)
        WHOLE  = WVLEN(1).gt.ZERO
        RED    = (.not.WHOLE).and.(WVLEN(NW).gt.ZERO)
        BLUE   = (.not.WHOLE).and.(WVLEN(NW).lt.ZERO)
      else
        COREWL = ZERO
        COREWN = ZERO
        ICORE  = 0
        WHOLE  = .false.
        RED    = .false.
        BLUE   = .false.
      end if
C     !END
      call BYE ('WUMBLE')
C
      return
      end
