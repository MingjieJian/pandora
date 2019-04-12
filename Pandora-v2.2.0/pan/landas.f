      subroutine LANDAS
     $(XIF,AF,KF,XIS,AS,KS,XI,A,K,KODE)
C
C     Rudolf Loeser, 1985 Jul 02
C---- Moves the appropriate Line Profile Integration data
C     into their slots in the Line Intensity data block.
C     (This is version 2 of LANDAS.)
C     !DASH
      save
C     !DASH
      real*8 A, AF, AS, XI, XIF, XIS
      integer K, KF, KODE, KS, LINT
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
      equivalence (LINKDS(13),LINT )
C     !DASH
C     !EJECT
      external MOVE1, CONMUL, HI, BYE
C
C               XIF(KF), XIS(KS), XI(K), AF(KF), AS(KS), A(K)
      dimension XIF(*),  XIS(*),  XI(*), AF(*),  AS(*),  A(*)
C
      call HI ('LANDAS')
C     !BEG
      if(LINT.gt.0) then
        KODE = 2
        call MOVE1  (XIF, KF, XI)
        call MOVE1  (AF,  KF, A )
        K = KF
      else
        KODE = 1
        call MOVE1  (XIS, KS, XI)
        call MOVE1  (AS,  KS, A )
        K = KS
      end if
C     !END
      call BYE ('LANDAS')
C
      return
      end
