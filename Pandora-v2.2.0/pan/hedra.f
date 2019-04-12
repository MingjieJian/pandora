      subroutine HEDRA
     $(XIFUL,KF,XISYM,KS,XIRED,KR,XIBLU,KB)
C
C     Rudolf Loeser, 1985 Jul 10
C---- Sets up line profile frequency integrations data tables.
C     (This is version 2 of HEDRA.)
C     !DASH
      save
C     !DASH
      real*8 XIBLU, XIFUL, XIRED, XISYM
      integer KB, KF, KR, KS, NOION
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
      equivalence (KZQ( 94),NOION)
C     !DASH
      external REFILL, HAPAX, HI, BYE
C
C               XISYM(KS), XIBLU(KB), XIRED(KR), XIFUL(KF)
      dimension XISYM(*),  XIBLU(*),  XIRED(*),  XIFUL(*)
C
      call HI ('HEDRA')
C     !BEG
      if(NOION.le.0) then
        call REFILL (XISYM,XIBLU,KB)
        call REFILL (XISYM,XIRED,KR)
        call HAPAX  (XIBLU,KB,XIRED,KR,XIFUL,KF,1)
      end if
C     !END
      call BYE ('HEDRA')
C
      return
      end
