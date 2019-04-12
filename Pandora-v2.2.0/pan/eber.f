      subroutine EBER
     $(XMIN,XMAX,TL,TU,NT,PZERO)
C
C     Rudolf Loeser, 1991 Dec 17
C---- Establishes a set of subintegral limits, for EULE.
C     (This is version 2 of EBER.)
C     !DASH
      save
C     !DASH
      real*4 HALF, PMAX, PMIN, PX, PZERO, TL, TU, X, XMAX, XMIN
      integer FLAG, I, NT
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
      external  PEYTRE, COMPR, LINER, HI, BYE
      intrinsic abs
C
C               TL(3), TU(3)
      dimension TL(*), TU(*)
C
      data HALF /5.E-1/
C     !EJECT
C
      call HI ('EBER')
C     !BEG
      XMIN = DL-SBM*CSN
      XMAX = DL+SBM*CSN
      X = -OFF
      if((X.gt.XMIN).and.(X.lt.XMAX)) then
        if(X.eq.DL) then
          NT    = 2
          TL(1) = XMIN
          TU(1) = DL
          TL(2) = DL
          TU(2) = XMAX
        else
          NT    = 3
          TL(1) = XMIN
          TU(3) = XMAX
          if(X.lt.DL) then
            TU(1) = X
            TL(2) = X
            TU(2) = DL
            TL(3) = DL
          else
            TU(1) = DL
            TL(2) = DL
            TU(2) = X
            TL(3) = X
          end if
        end if
      else
        NT    = 2
        TL(1) = XMIN
        TU(1) = DL
        TL(2) = DL
        TU(2) = XMAX
      end if
C
      call PEYTRE    ((R*abs(XMIN+OFF)),A,PMIN)
      call PEYTRE    ((R*abs(XMAX+OFF)),A,PMAX)
      call PEYTRE    ((R*abs(X   +OFF)),A,PX  )
      KOUNVC = KOUNVC+3
      call COMPR     (PMIN,PMAX,SBFQ,FLAG)
      if(FLAG.eq.0) then
        PZERO = HALF*(PMIN+PMAX)
        if(NT.le.1) then
          NT = 0
        else
          call COMPR (PZERO,PX,SBFQ,FLAG)
          if(FLAG.eq.0) then
            NT = 0
          end if
        end if
      end if
C     !EJECT
      if(LU.gt.0) then
        call LINER   (1,LU)
        write (LU,100) SBM,CSN,DL,XMIN,XMAX,X,PMIN,PMAX,PX
  100   format(' ','Script-M =',1PE14.6,', CSN =',E14.6,
     $             ', (DL =) Xk =',E14.6/
     $         ' ','xmin =',E14.6,', xmax =',E14.6,' -DV =',E14.6/
     $         ' ','Pmin =',E14.6,', Pmax =',E14.6,'  Px =',E14.6)
        if(NT.gt.0) then
          call LINER (1,LU)
          write (LU,101) (I,TL(I),TU(I),I=1,NT)
  101     format(' ',I1,'. subintegral from x =',1PE14.6,' to',E14.6)
        end if
      end if
C     !END
      call BYE ('EBER')
C
      return
      end
