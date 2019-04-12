      subroutine INDARO
     $(NSHL,NRPMX)
C
C     Rudolf Loeser, 1982 Apr 20
C---- Computes Shell Ray data.
C     !DASH
      save
C     !DASH
      integer N, NRPMX, NSHL, NTAN
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (KZQ( 45),NTAN )
C     !DASH
      external HI, BYE
C
      call HI ('INDARO')
C     !BEG
      NSHL  = (N-2)/NTAN+1
      NRPMX = 2*N+5
C     !END
      call BYE ('INDARO')
C
      return
      end
