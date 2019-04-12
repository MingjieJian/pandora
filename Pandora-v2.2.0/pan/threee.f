      subroutine THREEE
     $(IMAGE,ZAX,ZIN,NO,LINE)
C
C     Rudolf Loeser, 1968 Jul 30
C---- Prints a reference scale for ONE.
C     !DASH
      save
C     !DASH
      real*8 HALF, ONE, X, ZAX, ZERO, ZIN
      integer I, IY, JX, NH, NO, NV
      logical GOOD
      character BLANK*1, IMAGE*(*), LINE*117, MINUS*1, PLUS*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
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
C     !DASH
      external KINIT, KRIGIA, KLINEP, KPLOTP, KWHERE, KGIVE, HI, BYE
C
      data NV,NH /2, 117/
C     !EJECT
C
      call HI ('THREEE')
C     !BEG
C---- Initialize image
      call KINIT        (IMAGE,ZIN,ZAX,ZERO,ONE,NV,NH,BLANK,GOOD)
      if(.not.GOOD) then
        call KRIGIA     (ZIN,ZAX,ZERO,ONE,NV,NH)
      end if
      call KLINEP       (IMAGE,1,NV,NH,NV,MINUS,1)
      call KPLOTP       (IMAGE,1,NV,PLUS)
      X = ZIN
C---- Enter points
  100 continue
        X = X+ONE
        I = X
        if((I.gt.-10).and.(I.lt.+10)) then
          call KWHERE   (IMAGE,X,HALF,JX,IY)
          call KPLOTP   (IMAGE,JX,NV,PLUS)
          if(I.lt.0) then
            call KPLOTP (IMAGE,JX-1,1,MINUS)
            I = -I
          end if
          call KPLOTP   (IMAGE,JX,1,NUMBS(I+1))
        end if
      if(X.lt.ZAX) goto 100
C---- Print
      do 102 I = 1,NV
        call KGIVE      (IMAGE,I,LINE)
        write (NO,101) LINE
  101   format(' ',10X,A117)
  102 continue
C     !END
      call BYE ('THREEE')
C
      return
      end
