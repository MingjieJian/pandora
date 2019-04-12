      subroutine FRIGOR
     $(CPJ,XNUJ,KOOL, F)
C
C     Rudolf Loeser, 1980 Mar 17
C---- Computes F, for rates integrations.
C     !DASH
      save
C     !DASH
      real*8 CON, CPJ, F, XNUJ, XNUK
      logical KOOL
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
      equivalence (RZQ(  9),XNUK )
C     !DASH
      external RIGEL, HI, BYE
C
      call HI ('FRIGOR')
C     !BEG
      call RIGEL   (20,CON)
      F = CON*CPJ
      if(KOOL) then
        call RIGEL (13,CON)
        F = (F*CON)*(XNUK-XNUJ)
      end if
C     !END
      call BYE ('FRIGOR')
C
      return
      end
