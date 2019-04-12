      subroutine CUBANGO
     $(N,XLMXX,XLMDR,LLY,DRLMN,X,DR)
C
C     Rudolf Loeser, 2002 Sep 18
C---- Computes DR, the absorption fraction, for Hydrogen Lyman alpha
C     wing opacity.
C     (This is version 2 of CUBANGO.)
C     !DASH
      save
C     !DASH
      real*8 DR, DRLIM, DRLMN, X, XLMD2, XLMD3, XLMDR, XLMXC, XLMXP,
     $       XLMXX, dummy
      integer LLY, N
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
      equivalence (RZQ(162),XLMXC)
      equivalence (RZQ(163),XLMXP)
      equivalence (RZQ(164),XLMD2)
      equivalence (RZQ(154),XLMD3)
C     !DASH
      external DEAR, HI, BYE
C
C               XLMXX(LLY), XLMDR(LLY)
      dimension XLMXX(*),   XLMDR(*)
C
      call HI ('CUBANGO')
C     !BEG
      if(N.le.2) then
        DRLIM = XLMD2
      else if(N.eq.3) then
        DRLIM = XLMD3
      else
        DRLIM = DRLMN
      end if
C
      call DEAR (XLMXX, XLMDR, LLY, XLMXC, XLMXP, DRLIM, dummy, dummy,
     $           dummy, dummy, X, DR)
C     !END
      call BYE ('CUBANGO')
C
      return
      end
