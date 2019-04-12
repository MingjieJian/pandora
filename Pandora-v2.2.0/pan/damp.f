      subroutine DAMP
     $(YALSF,NO)
C
C     Rudolf Loeser, 2006 May 24
C---- Prints transitions data for RAMP.
C     !DASH
      save
C     !DASH
      real*8 YALSF
      integer I, IL, IU, IUL, KLIN, NO, NT
      character TITLE*13
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 5),NT )
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
      equivalence (LINKDS( 3),KLIN )
C     !DASH
C     !EJECT
      external PET, INTRANS, VAMP, HI, BYE
C
C               YALSF(NT)
      dimension YALSF(*)
C
      call HI ('DAMP')
C     !BEG
      do 101 I = 1,NT
        call PET     (I)
        if((KLIN.eq.1).or.(KLIN.eq.2)) then
          call INTRANS (IU, IL, 'DAMP', IUL)
          write (TITLE,100) IU,IL
  100     format('A for (',I2,'/',I2,')')
          call VAMP    (YALSF(IUL), TITLE, NO)
        end if
  101 continue
C     !END
      call BYE ('DAMP')
C
      return
      end
