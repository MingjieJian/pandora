      subroutine PENDA
     $(ION)
C
C     Rudolf Loeser, 1998 Mar 18
C---- Sets up an ion code, for diffusion calculations.
C     !DASH
      save
C     !DASH
      integer ION, IONST
      character QELSM*8
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
      equivalence (KZQ( 56),IONST)
      equivalence (QZQ(  2),QELSM)
C     !DASH
      external HI, BYE
C
      call HI ('PENDA')
C     !BEG
      ION = 0
      if(QELSM.eq.'H  ') then
        ION = 1
      else if(QELSM.eq.'HE ') then
        if(IONST.eq.1) then
          ION = 2
        else if(IONST.eq.2) then
          ION = 3
        end if
      end if
C     !END
      call BYE ('PENDA')
C
      return
      end
