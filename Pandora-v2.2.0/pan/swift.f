      subroutine SWIFT
     $(LU,CSF,BC,B,SN,CNDT,XND)
C
C     Rudolf Loeser, 2006 Feb 28
C---- Prints auxiliary LSF-calculation tables, for PSHAW.
C     !DASH
      save
C     !DASH
      real*8 B, BC, CNDT, CSF, SN, XND
      integer I, IL, IQINC, IQLBD, IU, LU, N, NL
      logical INCI
      character BLANK*1, TIT*4, V*16
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
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
      equivalence (IQQ( 51),IQINC)
      equivalence (IQQ(341),IQLBD)
C     !EJECT
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
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
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
C     !DASH
C     !EJECT
      external LINER, SHIM, HI, BYE
C
C               XND(N,NL), CNDT(N), CSF(N), SN(N), BC(N), B(N)
      dimension XND(N,*),  CNDT(*), CSF(*), SN(*), BC(*), B(*)
C
      call HI ('SWIFT')
C     !BEG
      if(LU.gt.0) then
        INCI = IQINC.gt.0
        TIT  = BLANK
        if(INCI) then
          TIT = 'CNDT'
        end if
C
        call LINER  (3, LU)
        write (LU,100) IU,IL
  100   format(' ','Intermediate functions for Line Source Function ',
     $             'calculation, transition (',I2,'/',I2,').'//
     $         ' ',' BC is the "Continuum Source Function as used in ',
     $             'the Line Source Function calculation;"'/
     $         ' ',' its value depends on options CSF, CSFB and ',
     $             'CSWITCH, as specified in [82 Sep 15].'/
     $         ' ',' (BC and CSF [= computed background source ',
     $              'function] pertain to line center [XI = 0].)')
        if(IQLBD.le.0) then
          write (LU,101)
  101     format(' ',' For additional related background data ',
     $               '(Sections "BACK (u/l)"), use option LBDPRNT.')
        end if
        call LINER  (2, LU)
        write (LU,102) IU,IL,TIT
  102   format(' ',18X,'CSF',14X,'BC',15X,'B',12X,'S(N)',11X,'N(',I2,
     $             ')',11X,'N(',I2,')',12X,A4)
        call LINER  (1, LU)
C
        do 105 I = 1,N
          V = BLANK
          if(INCI) then
            write (V,103) CNDT(I)
  103       format(1PE16.8)
          end if
C
          write (LU,104) I,CSF(I),BC(I),B(I),SN(I),XND(I,IU),
     $                   XND(I,IL),V
  104     format(' ',I5,1P6E16.8,A16)
          call SHIM (I, 5, LU)
  105   continue
      end if
C     !END
      call BYE ('SWIFT')
C
      return
      end
