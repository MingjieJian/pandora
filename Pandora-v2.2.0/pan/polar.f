      subroutine POLAR
     $(TE,DUMP,MESS,CI,OK)
C
C     Rudolf Loeser, 2005 Aug 31
C---- Controls calculation of CI(level 1) according to Voronov.
C     Returns OK = .false. if no data available.
C     !DASH
      save
C     !DASH
      real*8 A, CI, CK, DE, ERROR, P, TE, X, ZERO
      integer IONST
      logical DUMP, MESS, OK
      character QELSM*8
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
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ( 56),IONST)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external MOLAR, SOLAR, HI, BYE
C
      data ERROR /-1.D0/
C
      call HI ('POLAR')
C     !BEG
      CI = ZERO
C
      call MOLAR   (QELSM, IONST, MESS, DE, P, A, X, CK)
      OK = DE.ne.ERROR
      if(OK) then
        call SOLAR (TE, DE, P, A, X, CK, DUMP, CI)
      end if
C     !END
      call BYE ('POLAR')
C
      return
      end
