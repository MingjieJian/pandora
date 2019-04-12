      subroutine TAIGA
     $(N,K,H,RF)
C
C     Rudolf Loeser, 1989 Jul 05
C---- Checksums, for TOBOL.
C     (This is version 2 of TAIGA.)
C     !DASH
      save
C     !DASH
      real*8 H, RF
      integer IL, IOVER, ITER, IU, K, N
      character TIT*25
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
C     !EJECT
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
      equivalence (LEST( 3),ITER )
C     !DASH
      external CHECKER, HI, BYE
C
C               H(N,K), RF(N)
      dimension H(*),   RF(*)
C
      call HI ('TAIGA')
C     !BEG
      write (TIT,100) IU,IL,IOVER,ITER
  100 format(' ',I2,'/',I2,', ITER:',2I5)
C
      call CHECKER (H , 1, (N*K), 'Line Flux'//TIT)
      call CHECKER (RF, 1,  N   , 'Radiative Force'//TIT)
C     !END
      call BYE ('TAIGA')
C
      return
      end
