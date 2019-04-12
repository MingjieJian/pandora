      subroutine BET
     $(MODE,XNAME)
C
C     Rudolf Loeser, 1980 Nov 04
C---- Encodes (MODE=1) or decodes (MODE=2) a Continuum Block "name".
C     !DASH
      save
C     !DASH
      real*8 FACTS, XNAME
      integer MODE
C     !COM
C---- DWARF       as of 1997 Nov 19
      integer     KAKOD,KAKODS
      parameter   (KAKOD=4)
      dimension   KAKODS(KAKOD)
      common      /DWARF/ KAKODS
C     Continuum wavelength value type specification parameters.
C     (These parameters are packed and unpacked by "BET".)
C     !DASH
      external  GENISTA, HI, BYE
C
      dimension FACTS(KAKOD)
C
      data FACTS /1.D7, 1.D3, 1.D3, 1.D2/
C
      call HI ('BET')
C     !BEG
      call GENISTA (MODE, XNAME, KAKODS, FACTS, KAKOD, 'BET')
C     !END
      call BYE ('BET')
C
      return
      end
