      subroutine MYRON
     $(S,B,TAU,PROGLI,PGD,DL,SLF)
C
C     Rudolf Loeser, 1985 Jul 29
C---- Writes S, B, TAU for current transition, and other stuff,
C     to special Spectrum Save File.
C     (This is version 2 of MYRON.)
C     !DASH
      save
C     !DASH
      real*8 B, DL, PGD, PROGLI, S, SLF, TAU
      integer IL, IQPPU, IQSLV, IU, KTRN, LDL, LUSO, MODE, N
      character LAB*14
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (IQQ(151),IQPPU)
      equivalence (IQQ(294),IQSLV)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(28),LUSO )
C     !EJECT
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
      equivalence (LINKDS(12),LDL  )
      equivalence (LINKDS(20),KTRN )
C     !DASH
C     !EJECT
      external BUNT, PANT, PUNT, HI, BYE
C
C               TAU(N), S(N), B(N), PGD(3,LDL), SLF(N,KM), DL(KM)
      dimension TAU(*), S(*), B(*), PGD(*),     SLF(*),    DL(*)
C
      data MODE /1/
C
      call HI ('MYRON')
C     !BEG
      if(IQPPU.gt.0) then
C
        write (LUSO,100) IU,IL,KTRN,PROGLI
  100   format('----4  TRANSITION'/ 1P,
     $         2I10,2X,'Transition indices'/
     $         I10,2X,'# of wavelength points'/
     $         E15.7,2X,'PROGLI (input parameter)')
C
        write (LAB,101) IU,IL
  101   format(8X,2I3)
C
        call PUNT   (LUSO, DL,  KTRN,    MODE, 'DL '//LAB)
        call PUNT   (LUSO, PGD, (3*LDL), MODE, 'PGD'//LAB)
C
        call BUNT   (LUSO, TAU, 'TAU'//LAB)
        call BUNT   (LUSO, B,   'B  '//LAB)
        call BUNT   (LUSO, S,   'S  '//LAB)
C
        if(IQSLV.gt.0) then
          call PANT (LUSO, SLF, N, KTRN, MODE, 'SLF')
        end if
C
      end if
C     !END
      call BYE ('MYRON')
C
      return
      end
