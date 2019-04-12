      subroutine RESEDA
     $(N,NT,CWR,CHOP,CHLIM,ILI,NIL,TAUL,TRA,KTRAS,RHOJ,RHOP,RHOW,T,
     $ PTAU,WW,WT,WEIT)
C
C     Rudolf Loeser, 1980 May 02
C---- Drives DAISY, to compute "Combination RHOs".
C     (This is version 2 of RESEDA.)
C     !DASH
      save
C     !DASH
      real*8 CHLIM, CHOP, CWR, PTAU, RHOJ, RHOP, RHOW, T, TAUL, TRA,
     $       WEIT, WT, WW
      integer IL, ILI, IU, IUL, J, KLIN, KTRAS, M, N, NIL, NT
      logical DUMP, KILROY
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
      equivalence (LINKDS( 3),KLIN )
C     !DASH
C     !EJECT
      external ONE1, DALI, INTRANS, DAISY, PET, MASHED, HI, BYE
C
C               RHOW(N,NT), RHOP(N,NT), RHOJ(N,NT), TAUL(N,NT), TRA(N),
      dimension RHOW(N,*),  RHOP(N,*),  RHOJ(N,*),  TAUL(N,*),  TRA(*),
C
C               T(N), PTAU(N), WT(2*NIL+1), WW(N), WEIT(N)
     $          T(*), PTAU(*), WT(*),       WW(*), WEIT(*)
C
      call HI ('RESEDA')
C     !BEG
      M = 2*NIL+1
      KILROY = .true.
C
      call ONE1        (WT, M)
      call DALI        (CHLIM, DUMP, 'RESEDA')
C
      do 100 J = 1,NT
        call PET       (J)
        if(KLIN.eq.1) then
          call INTRANS (IU, IL, 'RESEDA', IUL)
          call DAISY   (N, IU, IL, CWR, CHOP, CHLIM, ILI, M, WT,
     $                  TAUL(1,IUL), TRA, KTRAS, RHOJ(1,IUL),
     $                  RHOP(1,IUL), RHOW(1,IUL), T, PTAU, WW, WEIT,
     $                  DUMP, KILROY)
        end if
  100 continue
C
      if(DUMP) then
        call MASHED    ('RESEDA')
      end if
C     !END
      call BYE ('RESEDA')
C
      return
      end
