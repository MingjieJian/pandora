      subroutine ISTUR
     $(XCBL)
C
C     Rudolf Loeser, 2002 Sep 23
C---- Checks whether this Continuum Data block pertains to a
C     Hydrogen Lyman line
C     !DASH
      save
C     !DASH
      real*8 XCBL
      integer ITYPE, KAK2, KAK3, KKLTIT
      logical LINE, lummy
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
      equivalence (QZQ(  2),QELSM)
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK( 1),KKLTIT)
C
C---- DWARF       as of 1997 Nov 19
      integer     KAKOD,KAKODS
      parameter   (KAKOD=4)
      dimension   KAKODS(KAKOD)
      common      /DWARF/ KAKODS
C     Continuum wavelength value type specification parameters.
C     (These parameters are packed and unpacked by "BET".)
      equivalence (KAKODS( 2),KAK2 )
      equivalence (KAKODS( 3),KAK3 )
C
C---- HILYLI      as of 2005 Dec 22
      integer     LYLINO
      common      /HILYLI/ LYLINO
C     Index of upper level, if this is an H Lyman line wavelength
C     (see subroutine ISTUR).
C     .
C     !DASH
C     !EJECT
      external PADDLE, BET, HI, BYE
C
C               XCBL(Miklen)
      dimension XCBL(*)
C
      data ITYPE /0/
C
      call HI ('ISTUR')
C     !BEG
      LYLINO = 0
C
      if(QELSM(:3).eq.'H  ') then
C
        call PADDLE (ITYPE, LINE, lummy)
        if(LINE) then
          call BET  (2, XCBL(KKLTIT))
          if(KAK3.eq.1) then
            LYLINO = KAK2
          end if
        end if
C
      end if
C     !END
      call BYE ('ISTUR')
C
      return
      end
