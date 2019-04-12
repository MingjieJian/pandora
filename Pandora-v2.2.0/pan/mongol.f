      subroutine MONGOL
     $(XLTIT,HEADER,BRIEF)
C
C     Rudolf Loeser, 1980 Nov 05
C---- Makes up a narrative line describing a Continuum Data Block,
C     from information packed into XLTIT.
C     !DASH
      save
C     !DASH
      real*8 XLTIT
      integer KAK1, KAK2, KAK3, KTYPE
      character BRIEF*10, HEADER*(*)
C     !COM
C---- DWARF       as of 1997 Nov 19
      integer     KAKOD,KAKODS
      parameter   (KAKOD=4)
      dimension   KAKODS(KAKOD)
      common      /DWARF/ KAKODS
C     Continuum wavelength value type specification parameters.
C     (These parameters are packed and unpacked by "BET".)
C     .
      equivalence
     $(KAKODS( 1),KAK1 ),(KAKODS( 2),KAK2 ),(KAKODS( 3),KAK3 ),
     $(KAKODS( 4),KTYPE)
C     .
C     !DASH
      external BET, TURKOIS, HI, BYE
C
      call HI ('MONGOL')
C     !BEG
      call BET     (2, XLTIT)
C
      call TURKOIS (KTYPE, KAK1, KAK2, KAK3, HEADER, BRIEF)
C     !END
      call BYE ('MONGOL')
C
      return
      end
