      subroutine RICE
     $(I,N,NL,NSL,M,KRJ,XND,NOK,RHOIJ,YBAR,X,IX,SLT)
C
C     Rudolf Loeser, 1978 Jun 27
C---- Provides for supplementary levels, for SECALE.
C     See also BUR!
C     !DASH
      save
C     !DASH
      real*8 ARHO, EPCBR, PS1, RHOIJ, SLT, SUM, X, XND, YBAR, ZERO
      integer I, IX, J, KRJ, M, N, NL, NSL
      logical NOK
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      equivalence (RZQ( 62),EPCBR)
C     !DASH
      external ARROW, DIVIDE, HI, BYE
C
      dimension X(*), IX(*)
C
C               XND(N,NL), RHOIJ(N,NT), YBAR(N,NT)
      dimension XND(N,*),  RHOIJ(*),    YBAR(*)
C     !EJECT
C
      call HI ('RICE')
C     !BEG
      SLT = ZERO
C
      if((NSL.gt.NL).and.NOK) then
        SUM = ZERO
C
        do 102 J = 1,NL
          if(J.ne.M) then
            call ARROW (I, J, 1, KRJ, RHOIJ, YBAR, X, IX, ARHO)
            SUM = SUM+XND(I,J)*ARHO
          end if
  102   continue
C
        PS1 = EPCBR*SUM
        call DIVIDE    ((-PS1), XND(I,M), SLT)
      end if
C     !END
      call BYE ('RICE')
C
      return
      end
