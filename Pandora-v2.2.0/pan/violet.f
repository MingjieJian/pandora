      subroutine VIOLET
     $(SSTAR,S,RHOS,CWJ,N,NT,RHOJ)
C
C     Rudolf Loeser, 1996 Jan 31
C---- Computes RHOJ.
C     (This is version 3 of VIOLET.)
C     !DASH
      save
C     !DASH
      real*8 CWJ, OMCWJ, ONE, R, RHOJ, RHOS, S, SSTAR, ZERO
      integer I, IL, IU, IUL, J, KLIN, N, NT
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
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external ZERO1, PET, INTRANS, DIVIDE, HI, BYE
C
C               RHOJ(N,NT), SSTAR(N,NT), S(N,NT), RHOS(N,NT)
      dimension RHOJ(N,*),  SSTAR(N,*),  S(N,*),  RHOS(N,*)
C
      call HI ('VIOLET')
C     !BEG
      call ZERO1          (RHOJ, (N*NT))
C
      OMCWJ = ONE-CWJ
C
      do 101 J = 1,NT
        call PET          (J)
        if(KLIN.eq.1) then
C
          call INTRANS    (IU, IL, 'VIOLET', IUL)
          do 100 I = 1,N
            if(SSTAR(I,IUL).ne.ZERO) then
              call DIVIDE (S(I,IUL), SSTAR(I,IUL), R)
              RHOJ(I,IUL) = (CWJ*R+OMCWJ)*RHOS(I,IUL)
            end if
  100     continue
C
        end if
  101 continue
C     !END
      call BYE ('VIOLET')
C
      return
      end
