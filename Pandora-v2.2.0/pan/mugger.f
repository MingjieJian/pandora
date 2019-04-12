      subroutine MUGGER
     $(N,NW,BKPC,BBC,CALLER)
C
C     Rudolf Loeser, 1986 Jul 11
C---- Dumps for FIFTY.
C     !DASH
      save
C     !DASH
      real*8 BBC, BKPC
      integer IL, IU, LUEO, N, NW
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
      external MESHED, ARROUT, MASHED, HI, BYE
C
C               BKPC(N,NW), BBC(N,NW)
      dimension BKPC(*),    BBC(*)
C
      call HI ('MUGGER')
C     !BEG
      call MESHED (CALLER, 2)
      write (LUEO,100) IU,IL
  100 format(' ','Dump of frequency-dependent COP and BC for ',
     $           'transition (',I2,',',I2,')',51X,'(Option FDBDMP)')
C
      call ARROUT (LUEO, BKPC, N, NW, 'COP')
      call ARROUT (LUEO, BBC,  N, NW, 'BC' )
      call MASHED (CALLER)
C     !END
      call BYE ('MUGGER')
C
      return
      end
