      subroutine FEDELM
     $(TAU,MSFT)
C
C     Rudolf Loeser, 1991 Jun 21
C---- Determines whether the static escape probability method must be
C     used, and sets MSFT accordingly.
C     !DASH
      save
C     !DASH
      real*8 EPTAU, TAU, ZERO
      integer MSFT
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
      equivalence (RZQ(118),EPTAU)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
C               TAU(N)
      dimension TAU(*)
C
      call HI ('FEDELM')
C     !BEG
      if(EPTAU.gt.ZERO) then
        if(TAU(2).gt.EPTAU) then
          MSFT = 2
        end if
      end if
C     !END
      call BYE ('FEDELM')
C
      return
      end
