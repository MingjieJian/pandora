      subroutine MORNEST
     $(LINE,LINK,LFB,LAB,FAB)
C
C     Rudolf Loeser, 2003 Mar 27
C---- Sets up a checksum label, for emergent continuum results.
C     !DASH
      save
C     !DASH
      integer IL, IU, LFB, LINK
      logical LINE
      character FAB*1, LAB*18
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
C     !EJECT
      external HI, BYE
C
      call HI ('MORNEST')
C     !BEG
      if(LINE) then
        write (LAB(:8),100) IU,IL
  100   format('(',I2,'/',I2,') ')
        if(LINK.eq.3) then
          LAB(9:18) = ' Line-free'
        else
          LAB(9:18) = 'Background'
        end if
C
      else
        LAB = 'Continuum         '
      end if
C
      if(LFB.eq.1) then
        FAB = 'f'
      else if(LFB.eq.2) then
        FAB = 'b'
      else
        FAB = '?'
      end if
C     !END
      call BYE ('MORNEST')
C
      return
      end
