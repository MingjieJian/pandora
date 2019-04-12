      subroutine ENTE
     $(X,SAVE,F,G)
C     Rudolf Loeser, 1991 Dec 17
C---- Computes P, G, and F=P*G at X, for ODEAR.
C     (This is version 2 of ENTE.)
C     !DASH
      save
C     !DASH
      real*4 F, G, P, SAVE, U, X, ZERO
C     !COM
C---- VOISTA      as of 1984 Apr 24
      integer     KOUNVC
      common      /VOISTA/ KOUNVC
C     Counts number of times Voigt Function is evaluated.
C     .
C---- ESEL        as of 2006 Jun 29
      integer     IU,IL,LU,KMAX,NMAX,NS,MUST
      real*4      DL,A,XNE,R,OFF,SBM,SBMN,SBMX,SBFQ,SMSK,CSN,CSN32
      common      /ESEL1/ DL,A,XNE,R,OFF,SBM,SBMN,SBMX,SBFQ,SMSK
      common      /ESEL2/ IU,IL,LU,KMAX,NMAX,NS,MUST
      common      /ESEL3/ CSN,CSN32
C     Parameters for EULE / ENTE / ODEAR (which computes R*4's)
C     (these are set up in subroutine FISON).
C     .
C     !DASH
      external  PEYTRE, ELLIK, EIKON, HI, BYE
      intrinsic abs
C
C               SAVE(NMAX,4)
      dimension SAVE(NMAX,*)
C
      data ZERO /0.E0/
C
      call HI ('ENTE')
C     !BEG
      if(MUST.eq.0) then
        U = ZERO
      else
        U = R*abs(X+OFF)
      end if
      call PEYTRE  (U, A, P)
      KOUNVC = KOUNVC+1
C
      call ELLIK   ((DL-X), G)
C
      if(NMAX.gt.0) then
        call EIKON (SAVE, NMAX, NS, X, U, P, G)
      end if
C
      F = P*G
C     !END
      call BYE ('ENTE')
C
      return
      end
