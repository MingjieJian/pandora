      subroutine MOTUGA
     $(M,J)
C
C     Rudolf Loeser, 1990 Jul 02
C---- Computes a dump index, for diffusion calculation.
C     !DASH
      save
C     !DASH
      integer IDFDI, J, M
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
      equivalence (KZQ(112),IDFDI)
C     !DASH
      external  HI, BYE
      intrinsic max
C
      call HI ('MOTUGA')
C     !BEG
      J = IDFDI
      if((J.lt.1).or.(J.gt.M)) then
        J = max((M/4),1)
      end if
C     !END
      call BYE ('MOTUGA')
C
      return
      end
