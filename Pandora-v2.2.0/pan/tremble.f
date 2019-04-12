      subroutine TREMBLE
     $(LU)
C
C     Rudolf Loeser, 2004 Apr 07
C---- Prints general LSF solution summary.
C     !DASH
      save
C     !DASH
      integer JSFEX, LU
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
      equivalence (KZQ(208),JSFEX)
C     !DASH
      external ABJECT, MELBERT, BELTERM, HI, BYE
C
      call HI ('TREMBLE')
C     !BEG
      if((LU.gt.0).and.(JSFEX.gt.0)) then
        call ABJECT  (LU)
C
C----   Tell the LSF story
        call MELBERT (LU)
C
C----   Tell the rest of the story
        call BELTERM (LU)
      end if
C     !END
      call BYE ('TREMBLE')
C
      return
      end
