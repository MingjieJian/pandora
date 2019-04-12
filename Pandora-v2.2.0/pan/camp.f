      subroutine CAMP
     $(YUSED,MESS)
C
C     Rudolf Loeser, 2006 May 24
C---- Encodes a message for A-printouts.
C     (This is version 2 of CAMP.)
C     !DASH
      save
C     !DASH
      real*8 Y, YUSED
      character MESS*24
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
      equivalence (RZQ(  8),Y    )
C     !DASH
      external HI, BYE
C
      call HI ('CAMP')
C     !BEG
      if(Y.ne.YUSED) then
        write (MESS,100) YUSED,Y
  100   format('Y-used',F5.2,:,' (input',F5.2,')')
      else
        write (MESS,100) YUSED
      end if
C     !END
      call BYE ('CAMP')
C
      return
      end
