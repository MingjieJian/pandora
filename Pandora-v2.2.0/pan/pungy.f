      subroutine PUNGY
     $(EP1,EP2,T,XND,GMI,RLI,RKI,CKI,CQS,RS,GNVL,KDGV,W)
C
C     Rudolf Loeser, 1990 Apr 12
C---- Computes Lyman epsilons by a "COMPLEX-UPPER-like" method.
C     !DASH
      save
C     !DASH
      real*8 CKI, CQS, EP1, EP2, GMI, GNVL, RKI, RLI, RS, T, TIN, TUT,
     $       W, XND
      integer IN, IPKL, IPLK1, IS, ISPKL, KDGV, KOLEV, MOX, N, NL, NSL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(40),NSL)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 33),KOLEV)
C     !DASH
      external SECOND, FAIFENA, PONGO, WGIVE, HI, BYE
C
      dimension W(*)
C
C               GMI(N,NSL), EP2(N), RKI(N,NSL), CKI(N,NSL), GNVL(N,NL),
      dimension GMI(*),     EP2(*), RKI(*),     CKI(*),     GNVL(*),
C
C               EP1(N), RLI(N,NSL), XND(N,NL), RS(N), CQS(N,NSL)
     $          EP1(*), RLI(*),     XND(*),    RS(*), CQS(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IPKL  ),(IN( 2),IPLK1 ),(IN( 3),ISPKL )
C     !EJECT
C
      call HI ('PUNGY')
C     !BEG
      call SECOND  (TIN)
C     (Get, and allocate, W allotment)
      call FAIFENA (IN, IS, MOX, 'PUNGY')
C
      call PONGO   (N, NL, KOLEV, KDGV, NSL, GMI, XND, RLI, CKI, RKI,
     $              CQS, GNVL, RS, W(IPKL), W(IPLK1), W(ISPKL),
     $              EP1, EP2)
C
C     (Give back W allotment)
      call WGIVE   (W, 'PUNGY')
C
      call SECOND  (TUT)
      T = TUT-TIN
C     !END
      call BYE ('PUNGY')
C
      return
      end
