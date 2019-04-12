      subroutine MURRE
     $(TE,DUMP,MESS,CI,OK)
C
C     Rudolf Loeser, 2004 Jul 29
C---- Controls calculation of CI(level 1) according to
C     Arnaud & Rothenflug. Returns OK = .false. if no data available.
C     (This is version 2 of MURRE.)
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CI, D, TE, XI, ZERO
      integer IONST, J
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
      external JANUS, WHEKLA, HI, BYE
C
      dimension XI(3), A(3), B(3), C(3), D(3)
C
      call HI ('MURRE')
C     !BEG
      CI = ZERO
C
      call JANUS    (QELSM, IONST, MESS, J, XI, A, B, C, D)
      OK = J.gt.0
      if(OK) then
        call WHEKLA (J, XI, A, B, C, D, TE, DUMP, CI)
      end if
C     !END
      call BYE ('MURRE')
C
      return
      end
