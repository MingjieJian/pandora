      subroutine CARY
     $(NL,ALPHA)
C
C     Rudolf Loeser, 1992 Jun 05
C---- Computes parameters ALPHA for GH-calculation, for TARPON.
C     !DASH
      save
C     !DASH
      real*8 ALPHA, CLVLS, ONE, X, ZERO
      integer I, NL
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
      equivalence (RZQ(134),CLVLS)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external SET1, HI, BYE
C
C               ALPHA(NL)
      dimension ALPHA(*)
C
      call HI ('CARY')
C     !BEG
      if(CLVLS.eq.ZERO) then
        call SET1 (ALPHA,NL,ONE)
      else
C
        do 100 I = 1,NL
          X = I
          ALPHA(I) = ONE/(X**CLVLS)
  100   continue
C
      end if
C     !END
      call BYE ('CARY')
C
      return
      end
