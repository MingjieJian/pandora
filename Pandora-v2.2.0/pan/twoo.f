      subroutine TWOO
     $(TS,M,TAUIJ,N,NT,BOT,TOP)
C
C     Rudolf Loeser, 1968 Jul 30
C---- Computes graph limits for ONE.
C     (Completely new, revised version.)
C     !DASH
      save
C     !DASH
      real*8 BOT, TAUIJ, TOP, TS, ZAX, ZIN
      integer IL, IMAX, IMIN, IU, IUL, J, KLIN, M, N, NT
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
      external  MINMAXD, TITAN, INTRANS, PET, HI, BYE
      intrinsic min, max
C
C               TS(M), TAUIJ(N,NT)
      dimension TS(*), TAUIJ(N,*)
C     !EJECT
C
      call HI ('TWOO')
C     !BEG
      call MINMAXD     (TS(2),1,(M-1),IMIN,IMAX)
      ZAX = TS(IMAX+1)
      ZIN = TS(IMIN+1)
C
      do 100 J = 1,NT
        call PET       (J)
        if(KLIN.eq.1) then
          call INTRANS (IU,IL,'TWOO',IUL)
          call MINMAXD (TAUIJ(2,IUL),1,(N-1),IMIN,IMAX)
          ZAX = max(ZAX,TAUIJ(IMAX+1,IUL))
          ZIN = min(ZIN,TAUIJ(IMIN+1,IUL))
        end if
  100 continue
C
      call TITAN       (ZIN,ZAX,BOT,TOP)
C     !END
      call BYE ('TWOO')
C
      return
      end
