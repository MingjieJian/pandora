      subroutine LINSEED
     $(LU)
C
C     Rudolf Loeser, 2002 Jun 06
C---- Prints the contents of LINUS.
C     !DASH
      save
C     !DASH
      integer I, IL, IU, LU
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
C     .
      equivalence
     $(LINKDS( 1),IU   ),(LINKDS( 2),IL   ),(LINKDS( 3),KLIN ),
     $(LINKDS( 4),ICE  ),(LINKDS( 5),IPRO ),(LINKDS( 6),METSE),
     $(LINKDS( 7),METSF),(LINKDS( 8),IBRSW),(LINKDS( 9),INKSW),
     $(LINKDS(10),LSFT ),(LINKDS(11),ILFLX),(LINKDS(12),LDL  ),
     $(LINKDS(13),LINT ),(LINKDS(14),LSFP ),(LINKDS(15),IFDB ),
     $(LINKDS(16),ISBG ),(LINKDS(17),KBT  ),(LINKDS(18),KRT  ),
     $(LINKDS(19),KST  ),(LINKDS(20),KTRN ),(LINKDS(21),LOML ),
     $(LINKDS(22),kds22)
C     .
C     !DASH
C     !EJECT
      external LINER, HI, BYE
C
      call HI ('LINSEED')
C     !BEG
      if(LU.gt.0) then
        call LINER (2,LU)
C
        write (LU,100) IU,IL
        write (LU,101) (LINKDS(I),I= 3,12)
        write (LU,102) (LINKDS(I),I=13,21)
C
  100   format(' ','Contents of LINUS: Line Source Function ',
     $             'calculation control switches for transition (',
     $             I2,'/',I2,') ---')
  101   format(' ',I8,' KLIN   - line "type" code (radiative, ',
     $                'passive, etc)'/
     $         ' ',I8,' ICE    - PRD calculation switch'/
     $         ' ',I8,' IPRO   - emergent profiles calculation switch'/
     $         ' ',I8,' METSE  - statistical equilibrium calculation ',
     $                'method selector'/
     $         ' ',I8,' METSF  - LSF calculation WN-matrix method ',
     $                'method selector (QR, RT, GR)'/
     $         ' ',I8,' IBRSW  - damping components selector'/
     $         ' ',I8,' INKSW  - input opacity signal'/
     $         ' ',I8,' LSFT   - LSF solution type code (full, ',
     $                'direct, etc)'/
     $         ' ',I8,' LDL    - number of line components')
  102   format(' ',I8,' LINT   - frequency integration range (half ',
     $                'vs. full)'/
     $         ' ',I8,' LSFP   - LSF printout switch'/
     $         ' ',I8,' IFDB   - LSF calculation background opacity ',
     $                'control (constant vs. varying)'/
     $         ' ',I8,' ISBG   - blended-line profile plot mode switch'/
     $         ' ',I8,' KBT    - length of input table XIBLUT'/
     $         ' ',I8,' KRT    - length of input table XIREDT'/
     $         ' ',I8,' KST    - length of input table XISYMT'/
     $         ' ',I8,' KTRN   - length of actual tables XI and DL'/
     $         ' ',I8,' LOML   - "line-background-continuum-opacity ',
     $                'control')
      end if
C     !END
      call BYE ('LINSEED')
C
      return
      end
