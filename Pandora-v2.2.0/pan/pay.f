      subroutine PAY
     $(IMAGE,XQ,IQ,SYM,KILROY)
C
C     Rudolf Loeser, 1982 May 05
C---- Plots a decade marker, if possible, for MAY.
C     !DASH
      save
C     !DASH
      real*8 TWO, XQ
      integer IQ, IY, JX, JXSAV, LEX
      logical KILROY
      character IMAGE*(*), MINUS*1, SYM*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(40),MINUS )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external  KWHERE, KPLOTP, HI, BYE
      intrinsic abs
C     !EJECT
C
      call HI ('PAY')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        JXSAV  = 0
      end if
C
C---- Compute plot position of XQ
      call KWHERE       (IMAGE,XQ,TWO,JX,IY)
C
      if((JX.ge.12).and.(JX.le.121)) then
C----   Plot marker
        call KPLOTP     (IMAGE,JX,1,SYM)
        if((JX-JXSAV).ge.3) then
          JXSAV = JX
C----     Plot label
C         If exponent negative, enter minus sign
          if(IQ.lt.0) then
            call KPLOTP (IMAGE,JX-1,2,MINUS)
          end if
C----     Plot exponent
          LEX = abs(IQ)
          call KPLOTP   (IMAGE,JX,2,NUMBS(LEX+1))
        end if
      end if
C     !END
      call BYE ('PAY')
C
      return
      end
