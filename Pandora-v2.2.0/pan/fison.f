      subroutine FISON
     $(DLK,AI,XNEI,IUU,ILL,RI,OFFI,LUU,MUSE,PHI,PD,KMX,SAVE,
     $ VEC,IPNT,NMX)
C
C     Rudolf Loeser, 1991 Dec 16
C---- Drives the calculation of a value of the line absorption
C     profile, for the case of Hydrogen with Stark broadening.
C
C     This calculation is done in single precision, and
C     FISON provides the r*8 -> r*4 - > r*8 interface.
C
C     (This is version 3 of FISON.)
C     !DASH
      save
C     !DASH
      real*8 AI, DLK, HSBFQ, HSBM, HSBMN, HSBMX, OFFI, PHI, PMSK, RI,
     $       XNEI
      real*4 P, PD, SAVE, VEC
      integer ILL, IPNT, IUU, KMX, LUU, MUSE, NMX
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ(120),HSBM )
      equivalence (RZQ(121),HSBMN)
      equivalence (RZQ(122),HSBMX)
      equivalence (RZQ(123),HSBFQ)
      equivalence (RZQ(124),PMSK )
C
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
C     !EJECT
      external EULE, HI, BYE
C
C               PD(9,KMX), SAVE(NMX,4), VEC(NMX), IPNT(NMX)
      dimension PD(*),     SAVE(*),     VEC(*),   IPNT(*)
C
      call HI ('FISON')
C     !BEG
      DL   = DLK
      A    = AI
      XNE  = XNEI
      R    = RI
      OFF  = OFFI
      SBM  = HSBM
      SBMN = HSBMN
      SBMX = HSBMX
      SBFQ = HSBFQ
      SMSK = PMSK
      IU   = IUU
      IL   = ILL
      MUST = MUSE
      LU   = LUU
C
      KMAX = KMX
      NMAX = NMX
      if(LU.le.0) then
        NMAX = 0
      end if
C
      call EULE (P, SAVE, PD, VEC, IPNT)
C
      PHI = P
C     !END
      call BYE ('FISON')
C
      return
      end
