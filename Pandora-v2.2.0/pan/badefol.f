      subroutine BADEFOL
     $(NO,KFUNC,KSTAR)
C
C     Rudolf Loeser, 1986 Feb 20
C---- Writes general heading, for BERRY.
C     (This is version 2 of BADEFOL.)
C     !DASH
      save
C     !DASH
      integer KFUNC, KSTAR, NO
      character BLANK*1, F*1, LINE*127, TEM*15
C     !COM
C---- LOOPER      as of 2006 May 05
      integer     NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      real*8      EMOO,WVLTRN
      logical     EXPAND,SPHERE,VXZERO,FLOBRD
      common      /LOOPER1/ NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      common      /LOOPER2/ EMOO,WVLTRN
      common      /LOOPER3/ EXPAND,SPHERE,VXZERO,FLOBRD
C
C     Emergent Profiles calculations control data (simplified version).
C
C     NVEL   : number of velocity tables
C     NVY    : current value of velocity-loop index, 1.le.NVY.le.NVEL
C              (i.e. index of current velocity set)
C     JVEL   : code describing current velocity set (i.e. KVEL(NVY) )
C              =     1 : VSB
C              =     2 : VXS
C              =     3 : VADD     (from AMDIFF and/or VELGRAD)
C              = 100+j : VXN(j)
C              = 200+j : VXN(j) + VADD
C              = 300+j : VAX(j)
C              = 400+j : VFB(j)
C              = 500+j : VFB(j) + VADD
C
C     LFBV   : number of viewing positions (front only, or back also)
C     LFB    : current value of views-loop index, 1 .le. LFB .le. LFBV
C              = 1 - front-face
C              = 2 - back-face
C
C     MF     : current value of look-angles-loop index, 1.le.MF.le.LF
C     MUK    : is .gt. 0 if line intensity profile must be printed
C              [when MUK > 0, then EMOO = EMU(MUK) ]
C     EMOO   : current value of look-angle
C     WVLTRN : wavelength (Angstroms) (i.e. at Delta-Lambda = 0).
C
C     VXZERO : tells whether the current velocity =0, or not
C     EXPAND : tells whether the procedures for expanding atmospheres
C              should be used (set up in SWEET)
C     SPHERE : tells whether this is a spherical, as opposed to
C              plane-parallel, atmosphere
C     FLOBRD : tells whether to use the flow-broadening procedure
C     .
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !EJECT
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
      external  LINER, ABORT, HI, BYE
C
      call HI ('BADEFOL')
C     !BEG
      if(NO.gt.0) then
        call LINER (1, NO)
        LINE = BLANK
C
        if(WAVENO) then
          write (NO,100)
  100     format(T14,'WN       = Wavenumber, in /cm (for Delta-',
     $                           'Lambda, turn option WAVENUMB = OFF)')
          LINE(12:13) = 'WN'
        else
          write (NO,101)
  101     format(T14,'DL       = Distance from Line Center, in ',
     $                           'Angstroms (for Wavenumber, turn ',
     $                           'option WAVENUMB = ON)')
          LINE(12:13) = 'DL'
        end if
C
        if(KFUNC.eq.1) then
          write (NO,102)
  102     format(T14,'I/A      = Intensity, in ',
     $                           'ergs/cm**2/s/sr/Angstrom'/
     $           T14,'I/Hz     = Intensity, in ergs/cm**2/s/sr/Hz')
          LINE(23:35) = '( F   KS  KI)'
          LINE(46:48) = 'I/A'
          LINE(61:64) = 'I/Hz'
          F = 'I'
        else
          if(SPHERE) then
            write (NO,103)
  103       format(T14,'SHLR     = (Shell Flux)/(Total Flux)')
            LINE(32:35) = 'SHLR'
          end if
          write (NO,104)
  104     format(T14,'F/A      = Flux 4*Pi*H at R(Star), '
     $                           'in ergs/cm**2/s/Angstrom'/
     $           T14,'F/Hz     = Flux 4*Pi*H at R(Star), '
     $                           'in ergs/cm**2/s/Hz')
          LINE(46:48) = 'F/A'
          LINE(61:64) = 'F/Hz'
          F = 'F'
        end if
C     !EJECT
        write (NO,105) F,F
  105   format(T14,'Residual = ',A,'/Hz divided by line-free ',A,
     $                         '/Hz, above')
        LINE(72:79) = 'Residual'
C
        if(KFUNC.eq.1) then
          write (NO,106)
  106     format(T14,'II       = Integrated Intensity, in ',
     $                           'ergs/cm**2/s/sr'/
     $           T14,'IIS      = II with 2*DL*(I/A) subtracted ',
     $                           '(Line without Continuum)')
          LINE( 92: 93) = 'II'
          LINE(107:109) = 'IIS'
        else
          write (NO,107)
  107     format(T14,'IF       = Integrated Flux, in ergs/cm**2/s'/
     $           T14,'IFS      = IF with 2*DL*(F/A) subtracted ',
     $                           '(Line without Continuum)')
          LINE( 92: 93) = 'IF'
          LINE(107:109) = 'IFS'
        end if
C
        write (NO,108)
  108   format(T14,'           (The above two integrated quantities ',
     $             'only for single lines, stationary case.)')
        write (NO,109)
  109   format(T14,'TB       = Brightness Temperature, in Kelvins')
        LINE(122:123) = 'TB'
C
        call LINER (3, NO)
C
        if(KSTAR.gt.0) then
          write (NO,110)
  110     format(' ',48X,'Radiation from illuminating star included.')
        end if
C
        if((KFUNC.eq.1).and.(.not.SPHERE)) then
          write (TEM,111) EMOO
  111     format('  Mu =',F7.4,'  ')
        else
          TEM = '---------------'
        end if
C
        write (NO,112) TEM,LINE
  112   format(' ',8('-------'),A15,8('-------')//
     $         ' ',A127)
C
        call LINER (1, NO)
      end if
C     !END
      call BYE ('BADEFOL')
C
      return
      end
