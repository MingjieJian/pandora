      subroutine ABELLIO
     $(HEADER,BRIEF)
C
C     Rudolf Loeser, 2002 Sep 26
C---- Encodes rates integration description data.
C     !DASH
      save
C     !DASH
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
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external HALT, HI, BYE
C
      call HI ('ABELLIO')
C     !BEG
      if(KTYPE.eq.5) then
        write (HEADER,100) KAK2,KAK1
  100   format('Lvl ',I2,' Continuum #',I7)
        write (BRIEF,101) KAK2,KAK1
  101   format('Lv',I2,'!',I5)
C
      else if(KTYPE.eq.27) then
        write (HEADER,102) KAK1
  102   format('Standard background #',I7)
        write (BRIEF,103) KAK1
  103   format('Stnd!',I5)
C
      else if(KTYPE.eq.12) then
        write (HEADER,104) KAK1
  104   format('K-Shell Continuum #',I7)
        write (BRIEF,105) KAK1
  105   format('KShl!',I5)
C
      else
        write (MSSLIN(1),106) KTYPE
  106   format('KTYPE =',I12,', which is neither 5, 12 nor 27.')
        call HALT ('ABELLIO', 1)
C
      end if
C     !END
      call BYE ('ABELLIO')
C
      return
      end
