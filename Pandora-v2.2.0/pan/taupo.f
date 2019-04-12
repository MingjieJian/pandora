      subroutine TAUPO
     $(IC,LINK)
C
C     Rudolf Loeser, 2004 Sep 03
C---- Sets up emergent background intensity calculation code for
C     line-specific calculations.
C     (This is version 3 of TAUPO.)
C     !DASH
      save
C     !DASH
      integer IC, ICE, LINK
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
      equivalence (LINKDS( 4),ICE  )
C     !DASH
C     !EJECT
      external HI, BYE
C
      call HI ('TAUPO')
C     !BEG
      LINK = 0
C
      if(IC.eq.1) then
        if(ICE.eq.0) then
          LINK = 1
        else
          LINK = 2
        end if
      else if(IC.eq.2) then
        LINK = 3
      end if
C     !END
      call BYE ('TAUPO')
C
      return
      end
