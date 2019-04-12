      subroutine TEPEE
     $(IU,IL,TIN)
C
C     Rudolf Loeser, 1980 May 23
C---- Concludes printouts for transition (IU/IL), for MINUET.
C     (This is version 2 of TEPEE.)
C     !DASH
      save
C     !DASH
      real*8 T, TIN, TOUT
      integer IL, IU, LSFP, MO
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
      equivalence (LINKDS(14),LSFP )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
C     !EJECT
      external LINER, SECOND, HI, BYE
C
      call HI ('TEPEE')
C     !BEG
      if((MO.gt.0).and.(LSFP.gt.0)) then
        call SECOND (TOUT)
        T = TOUT-TIN
C
        call LINER  (1, MO)
        write (MO,100) IU,IL,T
  100   format(' ','Total time for transition (',I2,'/',I2,'): ',
     $             1PE10.2,' sec.',7X,3('----------'))
      end if
C     !END
      call BYE ('TEPEE')
C
      return
      end
