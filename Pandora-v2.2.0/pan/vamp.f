      subroutine VAMP
     $(YUSED,TITLE,NO)
C
C     Rudolf Loeser, 2006 May 24
C---- Prints a line for A-TROUBLES summary.
C     !DASH
      save
C     !DASH
      real*8 Y, YUSED
      integer NO
      character TITLE*(*), YMSS*24
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
      external CAMP, HI, BYE
C
      call HI ('VAMP')
C     !BEG
      if(YUSED.ne.Y) then
        call CAMP (YUSED, YMSS)
        write (NO,100) TITLE,YMSS
  100   format(' ',A,T30,A)
      end if
C     !END
      call BYE ('VAMP')
C
      return
      end
