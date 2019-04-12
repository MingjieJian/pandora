      subroutine BUCKEYE
     $(TIN)
C
C     Rudolf Loeser, 1981 Apr 28
C---- Writes timing information, for TUBA.
C     (This is version 3 of BUCKEYE.)
C     !DASH
      save
C     !DASH
      real*8 TIME, TIN, TUT
      integer IL, IU, MO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
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
      external SECOND, LINER, HI, BYE
C     !EJECT
C
      call HI ('BUCKEYE')
C     !BEG
      call SECOND (TUT)
      TIME = TUT-TIN
      call LINER  (4,MO)
      write (MO,100) IU,IL,TIME
  100 format(' ','Post-Processing for transition',I3,'/',I2,
     $           ' took',F10.3,' sec.')
C     !END
      call BYE ('BUCKEYE')
C
      return
      end
