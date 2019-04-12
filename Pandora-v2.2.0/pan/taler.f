      subroutine TALER
     $(KILROY,LU,N,Z,TE,TAU,S,RHO,XJBAR,ST)
C
C     Rudolf Loeser, 2000 Jan 03
C---- Writes transition data to special file.
C     !DASH
      save
C     !DASH
      real*8 RHO, S, ST, TAU, TE, XJBAR, Z
      integer IL, IU, KLIN, LU, N
      logical KILROY
C     !COM
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
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
      equivalence (LINKDS( 3),KLIN )
C     !DASH
      external BUNT, PENT, HI, BYE
C
C               XJBAR(N), RHO(N), TAU(N), ST(N), TE(N), S(N), Z(N)
      dimension XJBAR(*), RHO(*), TAU(*), ST(*), TE(*), S(*), Z(*)
C     !EJECT
C
      call HI ('TALER')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        rewind LU
        write (LU,100) HEAD
  100   format(A80)
        write (LU,101) N
  101   format('N ( ',I4,' ) > ')
C
        call BUNT (LU,Z   ,'Z')
        call BUNT (LU,TE  ,'TE')
      end if
C
      write (LU,102) IU,IL,KLIN
  102 format(3I5)
C
      call PENT   (LU,TAU  ,IU,IL,'TAU')
      call PENT   (LU,S    ,IU,IL,'S')
      call PENT   (LU,RHO  ,IU,IL,'RHO')
      call PENT   (LU,XJBAR,IU,IL,'JBAR')
      call PENT   (LU,ST   ,IU,IL,'ST')
C     !END
      call BYE ('TALER')
C
      return
      end
