      subroutine DRECK
     $(IL,XR,N,DRLIMI)
C
C     Rudolf Loeser, 2003 Feb 25
C---- Sets up XR and DRLIMI except in H default cases.
C     !DASH
      save
C     !DASH
      real*8 DRLIM, DRLIMI, ONE, XR, ZERO
      integer IL, N
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
      equivalence (RZQ( 22),DRLIM)
      equivalence (QZQ(  2),QELSM)
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
C               DRLIMI(N)
      dimension DRLIMI(*)
C
      call HI ('DRECK')
C     !BEG
      if((QELSM(:3).eq.'H  ').and.(IL.eq.1)) then
        if(XR.ne.(-ONE)) then
          call SET1 (DRLIMI,N,XR)
        end if
      else
        if(XR.eq.(-ONE)) then
          XR = DRLIM
        end if
        call SET1   (DRLIMI,N,XR)
      end if
C     !END
      call BYE ('DRECK')
C
      return
      end
