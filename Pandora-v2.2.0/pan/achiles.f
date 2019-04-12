      subroutine ACHILES
     $(INTEGER,CHARACT)
C
C     Rudolf Loeser, 1980 Dec 08
C---- Returns, in "CHARACT" (type character), the character-equivalent
C     of the value of "INTEGER", for values in the range 0 to 100,
C     inclusive, suitable for printing with A3 format.
C     Returns *** for values out of range.
C     (This is version 3 of ACHILES.)
C     !DASH
      save
C     !DASH
      integer INTEGER
      character CHARACT*(*), NUMS*3
C     !DASH
      external HI, BYE
C
      dimension NUMS(101)
C
      data NUMS /
     $ '  0','  1','  2','  3','  4','  5','  6','  7','  8','  9',
     $ ' 10',' 11',' 12',' 13',' 14',' 15',' 16',' 17',' 18',' 19',
     $ ' 20',' 21',' 22',' 23',' 24',' 25',' 26',' 27',' 28',' 29',
     $ ' 30',' 31',' 32',' 33',' 34',' 35',' 36',' 37',' 38',' 39',
     $ ' 40',' 41',' 42',' 43',' 44',' 45',' 46',' 47',' 48',' 49',
     $ ' 50',' 51',' 52',' 53',' 54',' 55',' 56',' 57',' 58',' 59',
     $ ' 60',' 61',' 62',' 63',' 64',' 65',' 66',' 67',' 68',' 69',
     $ ' 70',' 71',' 72',' 73',' 74',' 75',' 76',' 77',' 78',' 79',
     $ ' 80',' 81',' 82',' 83',' 84',' 85',' 86',' 87',' 88',' 89',
     $ ' 90',' 91',' 92',' 93',' 94',' 95',' 96',' 97',' 98',' 99',
     $ '100'/
C
      call HI ('ACHILES')
C     !BEG
      if((INTEGER.ge.0).and.(INTEGER.le.100)) then
        CHARACT = NUMS(INTEGER+1)
      else
        CHARACT = '***'
      end if
C     !END
      call BYE ('ACHILES')
C
      return
      end
