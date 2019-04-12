      subroutine THESEUS
     $(KS,KR,KB,KF,KBX,KM)
C
C     Rudolf Loeser, 1985 Jul 02
C---- Manipulates the lengths of "XI...", the tables of line-profile
C     frequency-integration points.
C     (This is version 4 of THESEUS.)
C     !DASH
      save
C     !DASH
      integer IEXTR, KB, KBT, KBTMX, KBX, KF, KKS, KM, KMC, KMMAX, KMS,
     $        KMT, KR, KRT, KRTMX, KS, KSTMX, KT, LDLMX
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
      equivalence (KZQ(132),KMMAX)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(33),LDLMX)
      equivalence (LEST(60),KBTMX)
      equivalence (LEST(61),KRTMX)
      equivalence (LEST(62),KSTMX)
C
C---- NOTHER      as of 2004 Jun 25
      integer     NIAUGM
      parameter   (NIAUGM=3000)
C     (Be sure to recompile all users when changing NIAUGM ! )
C     Upper limit for total profile data points for coincident
C     background lines.
C     .
C     !DASH
      external  HI, BYE
      intrinsic min, max
C
      data IEXTR /2/
C
C     (IEXTR must be at least 2 for two reasons:
C     a) to be sure there is room for a 0, and
C     b) to be sure there is room for at least one extra value on
C        either side of zero.)
C     !EJECT
C
      call HI ('THESEUS')
C     !BEG
C---- Standard tables
      if(KR.le.0) then
        KR  = KS
      end if
      if(KB.le.0) then
        KB  = KS
      end if
      if(KBX.le.0) then
        KBX = KS
      end if
C
      KF  = KR+KB-1
      KMS = max((2*KS-1),((LDLMX*KF)+IEXTR))
C
C---- Transition-specific tables
C     (KRTMX, KBTMX, and KSTMX are estimates of maximum sizes that the
C     user specified in the input. These values were checked in HUDRO.)
      KRT = KRTMX
      KBT = KBTMX
      KKS = max(KS,KSTMX)
C
      if(KRT.le.0) then
        KRT = KKS
      end if
      if(KBT.le.0) then
        KBT = KKS
      end if
C
      KT  = KRT+KBT-1
      KMT = max((2*KKS-1),((LDLMX*KT)+IEXTR))
C
C---- Estimated maximum table size
      KMC = max(KMS,KMT)
C
C---- Set table length, for storage allocation (this limit is revised
C     once actual maximum size is known; q.v. GIRON & CRUMB).
      if(KMMAX.gt.0) then
        KM = max(KMC,KMMAX)
      else if(KMMAX.lt.0) then
        KM = min(KMC,-KMMAX)
      else
        KM = KMC
      end if
      KM = KM+NIAUGM
C     !END
      call BYE ('THESEUS')
C
      return
      end
