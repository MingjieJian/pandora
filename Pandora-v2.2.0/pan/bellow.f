      subroutine BELLOW
     $(KODE,LABEL)
C
C     Rudolf Loeser, 2006 Feb 24
C---- Makes a label for MYNAH.
C     !DASH
      save
C     !DASH
      integer IL, IU, KODE
      character LABEL*(*)
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
      external HI, BYE
C
      call HI ('BELLOW')
C     !BEG
      write (LABEL,100) IU,IL
  100 format('Line (',I3,'/',I2,') Central Optical Depth')
      if(KODE.eq.1) then
        LABEL(35:41) = ', PHI=1'
      else if(KODE.eq.2) then
        LABEL(35:39) = ', U=0'
      end if
C     !END
      call BYE ('BELLOW')
C
      return
      end
