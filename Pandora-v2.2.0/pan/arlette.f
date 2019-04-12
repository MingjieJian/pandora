      subroutine ARLETTE
     $(N,VEL,KVEL,ISB1,VSB,VXS)
C
C     Rudolf Loeser, 1990 May 10
C---- Modifies the velocity set if the current transition used
C     the Sobolev solution.
C     (This is version 3 of ARLETTE.)
C     !DASH
      save
C     !DASH
      real*8 VEL, VSB, VXS
      integer ISB1, KVEL, LSFT, N
C     !COM
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
C     !DASH
      external MOVE1, HI, BYE
C
C               VEL(N,NVEL), KVEL(NVEL), VXS(N), VSB(N)
      dimension VEL(N,*),    KVEL(*),    VXS(*), VSB(*)
C     !EJECT
C
      call HI ('ARLETTE')
C     !BEG
      if((LSFT.eq.2).and.(ISB1.gt.1)) then
        if(KVEL(1).ne.1) then
          call MOVE1 (VSB,N,VEL(1,1))
          KVEL(1) = 1
        end if
      else
        if(KVEL(1).ne.2) then
          call MOVE1 (VXS,N,VEL(1,1))
          KVEL(1) = 2
        end if
      end if
C     !END
      call BYE ('ARLETTE')
C
      return
      end
