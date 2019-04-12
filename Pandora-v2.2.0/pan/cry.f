      subroutine CRY
     $(LU)
C
C     Rudolf Loeser, 2004 Apr 20
C---- Gives a shout.
C     !DASH
      save
C     !DASH
      integer IONST, KEEOK, KHEOK, KOXOK, KX2OK, KX3OK, LU
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
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(72),KOXOK)
      equivalence (LEST(73),KHEOK)
      equivalence (LEST(77),KEEOK)
      equivalence (LEST(57),KX2OK)
      equivalence (LEST(58),KX3OK)
C     !DASH
C     !EJECT
      external SHOUT, HI, BYE
C
      call HI ('CRY')
C     !BEG
      if(LU.gt.0) then
        if((QELSM(1:2).eq.'O ').and.(IONST.eq.1)) then
          call SHOUT (LU, KOXOK, 'OXYGEN1')
        end if
        if((QELSM(1:2).eq.'O ').and.(IONST.eq.2)) then
          call SHOUT (LU, KX2OK, 'OXYGEN2')
        end if
        if((QELSM(1:2).eq.'O ').and.(IONST.eq.3)) then
          call SHOUT (LU, KX3OK, 'OXYGEN3')
        end if
        if((QELSM(1:2).eq.'HE').and.(IONST.eq.1)) then
          call SHOUT (LU, KEEOK, 'HELIUM1')
        end if
        if((QELSM(1:2).eq.'HE').and.(IONST.eq.2)) then
          call SHOUT (LU, KHEOK, 'HELIUM2')
        end if
      end if
C     !END
      call BYE ('CRY')
C
      return
      end
