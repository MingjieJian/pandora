      subroutine MEETOO
     $(BDIJ,JF,JL,NL,NO)
C
C     Rudolf Loeser, 1980 Oct 30
C---- Prints a complete B-ratios array.
C     (This is in effect a special version of 'SCRIBE'.)
C     (This is version 2 of MEETOO.)
C     !DASH
      save
C     !DASH
      real*8 BD, BDIJ
      integer I, IE, IL, IS, IU, JF, JL, KNT, NL, NO
      character BLANK*1, LAB*3
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  LINER, BRAT, HI, BYE
      intrinsic min
C
C               BDIJ(N,NL)
      dimension BDIJ(*)
C
      dimension BD(9)
C     !EJECT
C
      call HI ('MEETOO')
C     !BEG
      if(NO.gt.0) then
        IE = JF-1
  100   continue
          IS = IE+1
          IE = min(IE+9,JL)
C
          call LINER      (2, NO)
          write (NO,101) (I,I=IS,IE)
  101     format(' ',5X,'Depth',9I13)
          call LINER      (1, NO)
C
          LAB='u/l'
          do 105 IU = 2,NL
            do 104 IL = 1,(IU-1)
              KNT = 0
              do 102 I = IS,IE
                KNT = KNT+1
                call BRAT (I, IU, IL, BDIJ, BD(KNT))
  102         continue
              write (NO,103) LAB,IU,IL,(BD(I),I=1,KNT)
  103         format(' ',A3,I3,'/',I2,1X,1P9E13.5)
              LAB = BLANK
  104       continue
  105     continue
C
        if(IE.lt.JL) goto 100
      end if
C     !END
      call BYE ('MEETOO')
C
      return
      end
