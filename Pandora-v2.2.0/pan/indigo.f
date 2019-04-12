      subroutine INDIGO
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2005 Mar 18
C---- Allocates integer scratch storage for NUTIME.
C     (This is version 2 of INDIGO.)
C     !DASH
      save
C     !DASH
      integer IN, IS, KTRN, MUX
      character CALLER*(*)
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
      equivalence (LINKDS(20),KTRN )
C     !DASH
C     !EJECT
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('INDIGO')
C     !BEG
      call IGET (IS,  CALLER)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+KTRN
      MUX    = IN( 2)+KTRN
C
      call ILCK (MUX, CALLER)
C     !END
      call BYE ('INDIGO')
C
      return
      end
