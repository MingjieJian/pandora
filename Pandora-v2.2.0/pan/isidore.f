      subroutine ISIDORE
     $(DOPROF)
C
C     Rudolf Loeser, 2003 Mar 25
C---- Checks whether to compute the emergent line profile for the
C     current transition.
C     !DASH
      save
C     !DASH
      integer IPRO, KLIN
      logical DOPROF
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
      equivalence (LINKDS( 3),KLIN )
      equivalence (LINKDS( 5),IPRO )
C     !DASH
      external HI, BYE
C
      call HI ('ISIDORE')
C     !BEG
      DOPROF = ((KLIN.eq.1).or.(KLIN.eq.2)).and.(IPRO.gt.0)
C     !END
      call BYE ('ISIDORE')
C
      return
      end
