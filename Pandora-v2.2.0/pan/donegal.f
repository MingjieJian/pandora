      subroutine DONEGAL
     $(NL,AIJ,AATIJ,P,XNU)
C
C     Rudolf Loeser, 2006 Apr 20
C---- Computes forbidden As for all A(u,l)=0 transitions, for
C     calculation of CE (collisional excitation coefficient).
C     (This is version 2 of DONEGAL.)
C     !DASH
      save
C     !DASH
      real*8 AATIJ, AIJ, FROSC, P, XNU, ZERO
      integer IL, IU, JDCEA, NL
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
      equivalence (RZQ(129),FROSC)
C
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
      equivalence (MEST(19),JDCEA)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external CONCH, HI, BYE
C
C               AATIJ(NL,NL), AIJ(NL,NL), XNU(NSL), P(NSL)
      dimension AATIJ(NL,*),  AIJ(NL,*),  XNU(*),   P(*)
C
      call HI ('DONEGAL')
C     !BEG
      do 101 IU = 2,NL
        do 100 IL = 1,(IU-1)
          if(AIJ(IU,IL).eq.ZERO) then
            call CONCH (IU, IL, P, XNU, FROSC, AATIJ(IU,IL))
            JDCEA = JDCEA+1
          else
            AATIJ(IU,IL) = AIJ(IU,IL)
          end if
  100   continue
  101 continue
C     !END
      call BYE ('DONEGAL')
C
      return
      end
