      subroutine REEL
     $(NO,IMAGE,N,YSML,YBIG)
C
C     Rudolf Loeser, 1982 Apr 22
C---- Prints a plot image, for HALYS.
C     !DASH
      save
C     !DASH
      real*8 POW, TEN, TWO, YBIG, YSML
      integer IEX, J, K, N, NO, NUM
      character BLANK*1, IMAGE*(*), LINE*118, MINUS*1, PLUS*1, SLASH*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT(11),TEN   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(39),PLUS  )
      equivalence (SYMBS(40),MINUS )
      equivalence (SYMBS(41),SLASH )
C     !DASH
      external KGIVE, HI, BYE
C     !EJECT
C
      call HI ('REEL')
C     !BEG
      write (NO,100) N
  100 format(' ',9X,'1',115X,I2)
C
      NUM = 0
      IEX = YBIG+TWO
      do 104 K = 1,3
        IEX = IEX-2
        POW = TEN**IEX
        NUM = NUM+1
C
        call KGIVE   (IMAGE,NUM,LINE)
        write (NO,101) BLANK,BLANK,PLUS,POW,LINE
  101   format(' ',3A1,1PE6.0,A118)
C
        do 103 J = 1,8
          NUM = NUM+1
          call KGIVE (IMAGE,NUM,LINE)
          write (NO,102) LINE
  102     format(' ',9X,A118)
  103   continue
C
  104 continue
C
      IEX = IEX-2
      POW = TEN**IEX
      NUM = NUM+1
C
      call KGIVE     (IMAGE,NUM,LINE)
      write (NO,101) PLUS,SLASH,MINUS,POW,LINE
C
      do 106 K = 1,3
C
        do 105 J = 1,8
          NUM = NUM+1
          call KGIVE (IMAGE,NUM,LINE)
          write (NO,102) LINE
  105   continue
C
        IEX = IEX+2
        POW = TEN**IEX
        NUM = NUM+1
        call KGIVE   (IMAGE,NUM,LINE)
        write (NO,101) BLANK,BLANK,MINUS,POW,LINE
C
  106 continue
C     !END
      call BYE ('REEL')
C
      return
      end
