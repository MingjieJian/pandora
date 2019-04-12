      subroutine FLACK
     $(NO,KNLTE,WVL,IU,IL,ICE,ISB1)
C
C     Rudolf Loeser, 1986 Feb 20
C---- Writes detailed heading, for BERRY.
C     (This is version 4 of FLACK.)
C     !DASH
      save
C     !DASH
      real*8 APM, F, P, WVL
      integer ICE, IL, IQENH, IQIFF, IQSHF, ISB1, IU, KNLTE, LSFT, NO
      character BLANK*1, FACELAB*10, LABEL*38, PLAB*16, PRE*5, QLABEL*8,
     $          REM*25, TIT*5, TOT*24
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
C     !EJECT
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
      equivalence (IQQ(172),IQSHF)
      equivalence (IQQ( 38),IQENH)
      equivalence (IQQ(141),IQIFF)
C
C---- LINUS       as of 2004 May 12
      integer     LINKDS
      dimension   LINKDS(22)
      common      /LINUS/ LINKDS
C     Line source function calculation control parameters for the
C     current transition as set up by "PET" (and printed by "LINSEED").
C     IU    - index of upper level
C     IL    - index of lower level
C     KLIN  - line "type" code (1: radiative, 2: passive, etc)
C     ICE   - PRD calculation control
C     IPRO  - emergent profiles calculation control
C     METSE - statistical equilibrium calculation method selector
C     METSF - LSF calculation method selector (QR, RT, GR)
C     IBRSW - damping components selector
C     INKSW - input opacity signal
C     LSFT  - LSF solution code (0: full, 1:direct, etc)
C     ILFLX - line flux calculation control
C     LDL   - number of line components
C     LINT  - frequency integration range (half vs. full profile)
C     LSFP  - LSF printout control
C     IFDB  - LSF background control (constant vs. varying)
C     ISBG  - blended line profile plot mode switch
C     KBT   - length of input table XIBLUT
C     KRT   - length of input table XIREDT
C     KST   - length of input table XISYMT
C     KTRN  - length of actual tables XI and DL
C     LOML  - "line-background-continuum-opacity" control
C     ....  - (available)
      equivalence (LINKDS(10),LSFT )
C     !EJECT
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external SIPRFX, RIGHT, LOOT, PHOEBE, LINER, TUMBLE, HI, BYE
C
      dimension PLAB(2)
C
      data APM  /1.D10/
      data PLAB /'Kneer & Heasley.', 'Hubeny & Lites.'/
C
      call HI ('FLACK')
C     !BEG
      if(NO.gt.0) then
        call SIPRFX   ((WVL/APM), F, P, 2, PRE)
        call RIGHT    (PRE, TIT, 5)
        call PHOEBE   (0, QLABEL)
        call TUMBLE   (LFB, FACELAB)
        write (NO,100) QLABEL,IU,IL,WVL,F,TIT,FACELAB
  100   format(' ',A8,4X,'Profile of the ',I2,'/',I2,' Line at',
     $             1PE13.4,' Angstroms (=',0PF15.6,' ',A5,'meter)',
     $             20X,A10)
        call LINER    (1, NO)
C
        if(SPHERE) then
          write (NO,101)
  101     format(' ',T14,'Calculated using spherical coordinates.'/
     $           ' ',T14,'The total flux is the sum of a Shell ',
     $               'component and a Disk component.')
        else
          write (NO,102)
  102     format(' ',T14,'Calculated using plane-parallel coordinates.')
        end if
C     !EJECT
        REM = BLANK
        if(NVY.eq.0) then
          LABEL = 'Case: Flow-broadened'
          REM   = '(Note option FLWBPRNT)'
        else
          if(EXPAND) then
            call LOOT (TOT)
            LABEL = 'Case: Moving, '//TOT
          else
            LABEL = 'Case: Stationary'
          end if
        end if
        call LINER    (1, NO)
        write (NO,103) LABEL,REM
  103   format(' ',T14,A,5X,A)
C
        if((LSFT.eq.2).and.(ISB1.gt.1)) then
          write (NO,104)
  104     format(' ',T14,'Note that the Sobolev approximation ',
     $               'was used to solve the transfer equation.')
        end if
        call LINER (1, NO)
C
        if(KNLTE.le.0) then
          write (NO,105)
  105     format(' ',T14,'Calculated in LTE.')
        end if
C
        if((ICE.ne.0).and.(KNLTE.eq.1)) then
          write (NO,106) PLAB(ICE)
  106     format(' ',T14,'Partial Redistribution, using the ',
     $               'formulation of ',A)
          if((IQSHF.gt.0).and.EXPAND) then
            write (NO,107)
  107       format(' ',T14,'SNU-shift is used.')
          end if
        else
          write (NO,108)
  108     format(' ',T14,'Complete redistribution.')
        end if
C     !EJECT
        if(.not.SPHERE) then
          if(IQENH.gt.0) then
            write (NO,109)
  109       format(' ',T14,'R-squared Source Function ',
     $                 'enhancement included.')
          end if
          if(IQIFF.gt.0) then
            write (NO,110)
  110       format(' ',T14,'Front-face incident radiation affects the ',
     $                 'source function.')
          end if
        end if
      end if
C     !END
      call BYE ('FLACK')
C
      return
      end
