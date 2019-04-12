      subroutine SCREE
     $(DW,CDW)
C
C     Rudolf Loeser, 1983 Dec 05
C---- Sets up Reference value of Doppler Width.
C     !DASH
      save
C     !DASH
      real*8 CDW, DW
      integer NDW
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
      equivalence (KZQ(  1),NDW  )
C     !DASH
      external HI, BYE
C
C               DW(N)
      dimension DW(*)
C
      call HI ('SCREE')
C     !BEG
      CDW = DW(NDW)
C     !END
      call BYE ('SCREE')
C
      return
      end
