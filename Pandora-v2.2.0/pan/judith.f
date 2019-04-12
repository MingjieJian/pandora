      subroutine JUDITH
     $(K1,K2,K3,LTYP,XLTIT)
C
C     Rudolf Loeser, 1980 Nov 05
C---- Controls Continuum Block header code construction.
C     (This is version 2 of JUDITH.)
C     !DASH
      save
C     !DASH
      real*8 XLTIT
      integer K1, K2, K3, KAK1, KAK2, KAK3, KTYPE, LTYP
C     !COM
C---- SALKA       as of 1989 Nov 17
      character   GENLAB*60
      dimension   GENLAB(4)
      common      /SALKA/ GENLAB
C     Texts for error messages from GENISTA/AJUGA.
C     .
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
      external BET, HI, BYE
C
      call HI ('JUDITH')
C     !BEG
      KAK1 = K1
      KAK2 = K2
      KAK3 = K3
      KTYPE = LTYP
      GENLAB(4) = 'Continuum Data Block type code'
      call BET (1, XLTIT)
C     !END
      call BYE ('JUDITH')
C
      return
      end
