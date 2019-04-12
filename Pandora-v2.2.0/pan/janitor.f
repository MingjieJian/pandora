      subroutine JANITOR
     $(WN,N,IMAX)
C
C     Rudolf Loeser, 1989 Aug 15
C---- "Cleans out junk" from WN matrices.
C     !DASH
      save
C     !DASH
      real*8 WN, WNJNK, ZERO
      integer IMAX, J, N
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
      equivalence (RZQ(108),WNJNK)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external TRASH, HI, BYE
C
C               WN(N,N)
      dimension WN(N,*)
C
      call HI ('JANITOR')
C     !BEG
      if((N.gt.0).and.(IMAX.gt.0).and.(WNJNK.gt.ZERO)) then
        do 100 J = 1,N
          call TRASH (WN(1,J),IMAX,WNJNK)
  100   continue
      end if
C     !END
      call BYE ('JANITOR')
C
      return
      end
