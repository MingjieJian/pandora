      subroutine SHINE
     $(LL,K,N,ICE,IFDB,BCUL,COPUL,BC,XKPC,XJNU,SIG,T1,T2,T3,
     $ BKPC,BBC,PBC,PKPC,PJNU,PRXI,PSIG,PXRD,PZRD,PYRD)
C
C     Rudolf Loeser, 1980 Dec 17
C---- Gathers and sets up data for DIANA/ORION Data Blocks.
C     (This is version 3 of SHINE.)
C     !DASH
      save
C     !DASH
      real*8 BBC, BC, BCUL, BKPC, COPUL, PBC, PJNU, PKPC, PRXI, PSIG,
     $       PXRD, PYRD, PZRD, SIG, T1, T2, T3, XJNU, XKPC
      integer ICE, IFDB, IL, IU, K, LL, N
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
      external MOVE1, ZERO1, ONE1, SHEEN, ITCH, HI, BYE
C
C               BC(N), XKPC(N), BKPC(N,K), BBC(N,K), BCUL(N), PBC(N,K),
      dimension BC(*), XKPC(*), BKPC(N,*), BBC(N,*), BCUL(*), PBC(N,*),
C
C               PKPC(N,K), PJNU(N,K), PRXI(N,K), XJNU(N), T1(N), T2(N),
     $          PKPC(N,*), PJNU(N,*), PRXI(N,*), XJNU(N), T1(*), T2(*),
C
C               PZRD(N,K), PXRD(N,K), PYRD(N,K), COPUL(N), PSIG(N,K),
     $          PZRD(N,*), PXRD(N,*), PYRD(N,*), COPUL(*), PSIG(N,*),
C
C               T3(N), SIG(N)
     $          T3(*), SIG(*)
C
      call HI ('SHINE')
C     !BEG
      if(ICE.eq.0) then
        if(IFDB.gt.0) then
          call MOVE1 (BBC (1,LL), N, BC  )
          call MOVE1 (BKPC(1,LL), N, XKPC)
        else
          call MOVE1 (BCUL,       N, BC  )
          call MOVE1 (COPUL,      N, XKPC)
        end if
        call ZERO1   (XJNU,       N)
        call ZERO1   (SIG,        N)
        call ONE1    (T1,         N)
        call ZERO1   (T2,         N)
        call ZERO1   (T3,         N)
      else
        call MOVE1   (PBC (1,LL), N, BC  )
        call MOVE1   (PKPC(1,LL), N, XKPC)
        call SHEEN   (N, ICE, PJNU(1,LL), PRXI(1,LL), PSIG(1,LL),
     $                PXRD(1,LL), PZRD(1,LL), PYRD(1,LL),
     $                XJNU, SIG, T1, T2, T3)
      end if
C
C---- (Dump, if needed)
      call ITCH      ('SHINE', IU, IL, LL, K, N, XKPC, BC, SIG, XJNU,
     $                PRXI(1,LL), PXRD(1,LL), PZRD(1,LL), PYRD(1,LL))
C     !END
      call BYE ('SHINE')
C
      return
      end
