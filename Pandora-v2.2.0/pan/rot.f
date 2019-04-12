      subroutine ROT
     $(NO,K,L,WTAB,YNTN,INCRAD,YNTAN,PROGLI)
C
C     Rudolf Loeser, 1992 Aug 19
C---- Makes a graph of an absolute intensity profile.
c     (This is version 3 of ROT.)
C     !DASH
      save
C     !DASH
      real*8 BOT, PROGLI, TOP, WTAB, XL, XU, YLL, YNTAN, YNTN, YUL,
     $       ZERO, dummy
      integer IL, IPEX, IU, K, L, LUEO, NO
      logical DUMP, INCRAD
      character BLANK*1, LINE*99, QLABEL*8, STAR*1, TIT*24
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
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(45),STAR  )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !EJECT
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
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
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
C     !EJECT
C---- BASH        as of 1984 Apr 19
      integer            JXLOBA,JXHIBA,JYLOBA,JYHIBA,NTHRBA,NPOIBA
      common      /BASH/ JXLOBA,JXHIBA,JYLOBA,JYHIBA,NTHRBA,NPOIBA
C     Control data for "BEIGE".
C     .
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
      external LOOT, TITAN, LINER, MELIC, ONION, GABOR, PHOEBE, LUTALE,
     $         CHROME, KPRINT, ABJECT, TUMBLE, GALENA, MESHED, MASHED,
     $         TOMATO, DANIEL, HI, BYE
C
C               WTAB(KM), YNTN(KM,L), YNTAN(KM)
      dimension WTAB(*),  YNTN(*),    YNTAN(*)
C
      call HI ('ROT')
C     !BEG
      if(NO.gt.0) then
        DUMP = (IPEX.lt.0).or.(IPEX.eq.4)
C
C-----  Initialize output-related stuff
        call LOOT     (TIT)
        call PHOEBE   (0, QLABEL)
        write (LINE,100) WLAB3,IU,IL,QLABEL,TIT
  100   format('Log(I/Hz) vs. ',A,', for the (',I2,'/',I2,') line ',
     $         'of ',A8,5X,A24)
        call TUMBLE   (LFB, LINE(90:99))
C
C----   Set up ordinate limits
        YUL = -ZZLARGE
        YLL = +ZZLARGE
        call CHROME   (K, L, YNTN,  0, dummy, YUL, YLL)
        if(INCRAD) then
          call CHROME (K, 1, YNTAN, 0, dummy, YUL, YLL)
        end if
C
        if(DUMP) then
          call MESHED ('ROT', 2)
          call GALENA (IU, IL, K, L, INCRAD, NVY, WTAB, YLL, YUL,
     $                 YNTN)
        end if
C     !EJECT
        if((YUL.gt.YLL).and.(YUL.gt.ZERO).and.(YLL.gt.ZERO)) then
C
          call LUTALE   (WTAB, YNTN, K, PROGLI, XL, XU)
          call TITAN    (YLL, YUL, BOT, TOP)
C----     Set up graph
          call MELIC    (IMAGE, XL, XU, BOT, TOP)
C----     Enter normal intensities
          call ONION    (K, L, WTAB, YNTN,  IMAGE, BLANK, 1)
C----     Enter intensities corrected for additional illumination
          if(INCRAD) then
            call ONION  (K, 1, WTAB, YNTAN, IMAGE, STAR,  2)
          end if
C
          if(DUMP) then
            call LINER  (1, LUEO)
            write (LUEO,101) NPOIBA,NTHRBA/2
  101       format(' ','RED',5X,'NPOIBA=',I6,3X,'NTHRBA/2=',I4)
          end if
          if((NPOIBA.gt.(NTHRBA/2)).or.(PROGLI.ne.ZERO)) then
C----       Print graph
            call ABJECT (NO)
            write (NO,102) LINE
  102       format(' ',A)
            call LINER  (1, NO)
            call KPRINT (IMAGE, NO)
            call TOMATO (NO, XL, XU)
          else
            call GABOR  (NO, 3, 0, LINE)
          end if
        else
          call DANIEL   (NO, LINE, YUL, YLL)
        end if
C
        if(DUMP) then
          call MASHED   ('ROT')
        end if
      end if
C     !END
      call BYE ('ROT')
C
      return
      end
