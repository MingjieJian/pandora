      subroutine ESTEEM
     $(IU,IL,METSE)
C
C     Rudolf Loeser, 2006 Mar 06
C---- Sets those METSE values for which no input was found.
C     !DASH
      save
C     !DASH
      integer IL, IU, LACK, METSE, MSEDG, MSEDW
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
      equivalence (KZQ(221),MSEDG)
      equivalence (KZQ(222),MSEDW)
C     !DASH
      external HI, BYE
C
      data LACK /-317/
C
      call HI ('ESTEEM')
C     !BEG
      if(METSE.eq.LACK) then
        if(IL.eq.1) then
          METSE = MSEDW
        else
          METSE = MSEDG
        end if
      end if
C     !END
      call BYE ('ESTEEM')
C
      return
      end
