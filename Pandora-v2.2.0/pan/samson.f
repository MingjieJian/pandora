      subroutine SAMSON
     $(NO,NRAD,LABIJ)
C
C     Rudolf Loeser, 1980 OCT 30
C---- Prints labels, for DITHER.
C     (This is version 2 of SAMSON.)
C     !DASH
      save
C     !DASH
      integer I, IE, IL, IS, IU, KARP, LABIJ, NO, NRAD
      character BLANK*1, LINE*120, SYM*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  GARLIC, HI, BYE
      intrinsic min
C
C               LABIJ(NRAD)
      dimension LABIJ(*)
C
      call HI ('SAMSON')
C     !BEG
      IE = 0
  100 continue
        IS = IE+1
        IE = min(IE+10,NRAD)
C
        LINE = BLANK
        KARP = 0
        do 102 I = IS,IE
          IU = LABIJ(I)/100
          IL = LABIJ(I)-100*IU
          call GARLIC (I, ALPHS, 26, SYM)
C
          KARP = KARP+12
          write (LINE(KARP-11:KARP),101) IU,IL,SYM
  101     format('[',I2,'/',I2,']: ',A1,', ')
  102   continue
C
        LINE(KARP-1:KARP-1) = BLANK
        write (NO,103) LINE
  103   format(' ',1X,A120)
C
      if(IE.lt.NRAD) goto 100
C     !END
      call BYE ('SAMSON')
C
      return
      end
