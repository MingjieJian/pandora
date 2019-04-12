      subroutine ELLIK
     $(X,ELL)
C
C     Rudolf Loeser, 1991 Dec 16
C---- Computes a value of the normalized Hydrogen Stark profile,
C     for EULE.
C     !DASH
      save
C     !DASH
      real*4 D, ELL, FAC, POW, X
C     !COM
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
      external  HI, BYE
      intrinsic abs
C
      data FAC /7.5E-1/
      data POW /2.5E0/
C
      call HI ('ELLIK')
C     !BEG
      D   = (CSN+abs(X))**POW
      ELL = FAC*(CSN32/D)
C     !END
      call BYE ('ELLIK')
C
      return
      end
