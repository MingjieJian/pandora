      subroutine TATAR
     $(NP)
C
C     Rudolf Loeser, 1981 Oct 26
C---- Computes current value of NP, given the previous one.
C     Set NP=0 on input to start series afresh.
C     Returns with NP=0 if all values have been used.
C---- The series of NP values picks out the indices of those
C     depths for which ray matrices will be computed.
C     !DASH
      save
C     !DASH
      integer N, NP, NTAN
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
      call HI ('TATAR')
C     !BEG
      if(NP.le.0) then
        NP = N
      else
C
        NP = NP-NTAN
        if(NP.lt.2) then
          NP = 0
        end if
C
      end if
C     !END
      call BYE ('TATAR')
C
      return
      end
