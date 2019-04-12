      subroutine BENWIG
     $(I,KODE)
C
C     Rudolf Loeser, 1982 Mar 30
C---- Writes error message for final processing of Diana Data Blocks.
C     !DASH
      save
C     !DASH
      integer I, IL, IU, KODE, LUEO
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
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
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external LINER, HALT, HI, BYE
C
      call HI ('BENWIG')
C     !BEG
      call LINER (1, LUEO)
      write (LUEO,100) IU,IL,I,KODE
  100 format(' ','Transition (',I2,',',I2,'), term omitted from ',
     $           'frequency sum',10X,'(IND=',I2,', KODE=',I2,').')
C
      if(KODE.eq.1) then
        write (LUEO, 101)
  101   format(' ',5X,'TNU bad.')
      else if(KODE.eq.2) then
        write (LUEO, 102)
  102   format(' ',5X,'WN bad.')
C
      else
        write (MSSLIN(1),103) KODE
  103   format('KODE =',I12,', which is not 1 or 2.')
        call HALT ('BENWIG', 1)
      end if
C     !END
      call BYE ('BENWIG')
C
      return
      end
