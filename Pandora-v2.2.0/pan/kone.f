      subroutine KONE
     $(XNUJ,KSHL,RJ,Z)
C
C     Rudolf Loeser, 1980 Mar 11
C---- Computes RJ and Z, for VOOM.
C     !DASH
      save
C     !DASH
      real*8 CON4, CON6, CON7, DNUJ, RJ, RWKSI, XNUJ, XNUK, Z
      logical KSHL
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
      equivalence (RZQ(  3),RWKSI)
C     !DASH
      external RIGEL, HI, BYE
C
      call HI ('KONE')
C     !BEG
      DNUJ = XNUK-XNUJ
      if(KSHL) then
        call RIGEL (6,CON6)
        RJ = CON6/(RWKSI**3)
      else
        call RIGEL (7,CON7)
        RJ = CON7*(DNUJ**3)
      end if
      call RIGEL   (4,CON4)
      Z = DNUJ*CON4
C     !END
      call BYE ('KONE')
C
      return
      end
