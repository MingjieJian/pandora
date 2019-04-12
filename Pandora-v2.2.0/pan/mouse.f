      subroutine MOUSE
     $(N,NR)
C
C     Rudolf Loeser, 1970 Feb 11
C---- Determines table lengths for Absorption Profile Analysis.
C     !DASH
      save
C     !DASH
      integer LIM, N, NANA1, NANA2, NR
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
      equivalence (KZQ(135),NANA1)
      equivalence (KZQ(137),NANA2)
C     !DASH
      external  HI, BYE
      intrinsic min
C
      data LIM /10/
C
      call HI ('MOUSE')
C     !BEG
      NR = min(((N+1-NANA1)/NANA2+1),LIM)
C     !END
      call BYE ('MOUSE')
C
      return
      end
