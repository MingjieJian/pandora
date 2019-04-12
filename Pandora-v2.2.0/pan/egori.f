      subroutine EGORI
     $(DLL,K,XNEE,NR,IUU,ILL,ELL)
C
C     Rudolf Loeser, 1991 Dec 26
C---- Sets up Stark profile to be exhibited by PEEK,
C
C     which involves conversions from real*8 to real*4 to real*8.
C
C     (This is version 3 of EGORI.)
C     !DASH
      save
C     !DASH
      real*8 DLL, ELL, XNEE
      real*4 EL, X, ZERO
      integer I, ILL, IUU, J, K, NR
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
      external COUSIN, ELLIK, HI, BYE
C
C               DLL(KM), XNEE(N), ELL(N ,KM)
      dimension DLL(*),  XNEE(*), ELL(NR,*)
C
      data ZERO /0.E0/
C
      call HI ('EGORI')
C     !BEG
      IU = IUU
      IL = ILL
      DL = ZERO
C
      do 101 I = 1,NR
        XNE = XNEE(I)
        call COUSIN
        do 100 J = 1,K
          X = DLL(J)
          call ELLIK (X,EL)
          ELL(I,J) = EL
  100   continue
  101 continue
C     !END
      call BYE ('EGORI')
C
      return
      end
