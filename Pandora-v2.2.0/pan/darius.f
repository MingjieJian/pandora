      subroutine DARIUS
     $(KFUNC,WVL,LDL,DDL,INCRAD,K,KLIN,KILROY)
C
C     Rudolf Loeser, 1981 May 05
C---- Writes profile heading to special Spectrum Save file.
C     !DASH
      save
C     !DASH
      real*8 DDL, EM, WVL, ZERO
      integer I, ICE, IL, INCRAD, IU, K, KFUNC, KLIN, LDL, LUSO
      logical KILROY
      character TIT*3
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
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(28),LUSO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      equivalence (LINKDS( 4),ICE  )
C     !DASH
C     !EJECT
      external HALT, HI, BYE
C
C               DDL(LDLMX)
      dimension DDL(*)
C
      dimension TIT(2)
C
      data TIT /'INT', 'FLX'/
C
      call HI ('DARIUS')
C     !BEG
      if((KFUNC.lt.1).or.(KFUNC.gt.2)) then
        write (MSSLIN(1),100) KFUNC
  100   format('KFUNC =',I12,', which is neither 1 nor 2.')
        call HALT ('DARIUS', 1)
      end if
C
      if(KILROY) then
        KILROY = .false.
        write (LUSO,101) IU,IL,KLIN,LFBV,JVEL,WVL
  101   format('---=5  TRANSITION'/ 1P,
     $         2I10,2X,'Transition indices'/
     $         I10,2X,'KLIN =1 for radiative, =2 for passive'/
     $         I10,2X,'LFBV = # of view positions (=1,2: front,back)'/
     $         I10,2X,'JVEL = velocity table descriptor'/
     $         E20.12,2X,'Wavelength/wavenumber')
        write (LUSO,102) LDL
  102   format(I10,2X,'LDL = # of line components')
        write (LUSO,103) (DDL(I),I=1,LDL)
  103   format(F20.8,2X,'line component offset (Angstroms)')
      end if
C
      EM = EMOO
      if(KFUNC.eq.2) then
        EM = ZERO
      end if
C
      write (LUSO,104) TIT(KFUNC),EM,KFUNC,ICE,INCRAD,LFB,
     $                 NVY,EXPAND,K
  104 format('----1  LINE PROFILE: ',A3/ 1P,
     $       E20.12,2X,'Mu (cosine of look-angle)'/
     $       I10,2X,'KFUNC =1 means: INTensity, =2 means: FLuX'/
     $       I10,2X,'ICE .eq. 0 means: complete redistribution, ',
     $              '.ne. 0 means: partial redistribution'/
     $       I10,2X,'INCRAD =1 if there is incident radiation, ',
     $              '=0 if not'/
     $       I10,2X,'LFB =1 means: front-face, =2 means: back-face'/
     $       I10,2X,'NVY = value of the index of loop ',
     $              'over velocities ("0" = flow-broadened profile)'/
     $       L10,2X,'EXPAND, tells which procedures are used'/
     $       I10,2X,'# of profile points, = # of lines in the ',
     $              'following table:')
C     !EJECT
      if(KFUNC.eq.1) then
        write (LUSO,105)
  105   format(7X,'Each line has: i,DL,WN,TC/Hz,I/Hz,I/A,(F KS KI),',
     $            'KODE'/
     $         7X,'I/Hz,I/A,(F KS KI) as in Printout'/
     $         7X,'i = line number'/
     $         7X,'TC/Hz = background intensity'/
     $         7X,'DL = delta lambda (Angstroms)'/
     $         7X,'WN = wavenumber'/
     $         7X,'KODE = error code (as printed just to the right ',
     $            'of (F KS KI) if nonzero)')
      else
        if(SPHERE) then
          write (LUSO,106)
  106     format(7X,'Each line has: i,DL,WN,TC/Hz,F/Hz,F/A,TF,SF'/
     $           7X,'F/Hz,F/A as in Printout'/
     $           7X,'i = line number'/
     $           7X,'TC/Hz = background flux'/
     $           7X,'DL = delta lambda (Angstroms)'/
     $           7X,'WN = wavenumber'/
     $           7X,'TF = total flux'/
     $           7X,'SF = shell flux')
        else
          write (LUSO,107)
  107     format(7X,'Each line has: i,DL,WN,TC/Hz,F/Hz,F/A'/
     $           7X,'F/Hz,F/A as in Printout'/
     $           7X,'i = line number'/
     $           7X,'TC/Hz = background flux'/
     $           7X,'DL = delta lambda (Angstroms)'/
     $           7X,'WN = wavenumber')
        end if
      end if
C     !END
      call BYE ('DARIUS')
C
      return
      end
