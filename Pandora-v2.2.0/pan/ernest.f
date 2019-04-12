      subroutine ERNEST
     $(LU,KODE,IJECT)
C
C     Rudolf Loeser, 1989 May 08
C---- Prints a transition heading.
C     (IJECT is for DEJECT.)
C     !DASH
      save
C     !DASH
      integer IJECT, IL, IU, KODE, LU, NC
      character LINE*12
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
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
C     !DASH
      external LOTHAR, PRIAM, HI, BYE
C
      call HI ('ERNEST')
C     !BEG
      IJECT = 0
      if(LU.gt.0) then
        call LOTHAR (IU, IL, KODE, LINE, NC)
        call PRIAM  (LU,           LINE, NC)
        IJECT = 1
      end if
C     !END
      call BYE ('ERNEST')
C
      return
      end
