      subroutine BLAND
     $(XLB1,TAU,NRAD,N,NT,LABEL)
C
C     Rudolf Loeser, 1976 Jul 01
C---- Gathers TAU sets for radiative transitions from
C     Line Intensity Data Blocks.
C     !DASH
      save
C     !DASH
      real*8 TAU, XLB1, dummy
      integer I, KLIN, LABEL, MMNAM, MMTAU, N, NRAD, NT
C     !COM
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(16),MMTAU)
      equivalence (MML( 1),MMNAM)
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
      equivalence (LINKDS( 3),KLIN )
C     !DASH
      external PET, LIDGET, MOVE1, HI, BYE
C
C               XLB1(Li1len), TAU(N,NT), LABEL(NT)
      dimension XLB1(*),      TAU(N,*),  LABEL(*)
C     !EJECT
C
      call HI ('BLAND')
C     !BEG
      NRAD = 0
      do 100 I = 1,NT
        call PET      (I)
        if(KLIN.eq.1) then
          NRAD = NRAD+1
          call LIDGET (XLB1,1,dummy,0,dummy,0,I)
          call MOVE1  (XLB1(MMTAU),N,TAU(1,NRAD))
          LABEL(NRAD) = XLB1(MMNAM)
        end if
  100 continue
C     !END
      call BYE ('BLAND')
C
      return
      end
