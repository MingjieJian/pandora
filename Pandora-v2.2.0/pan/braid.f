      subroutine BRAID
     $(NO,EMUI,W,WV,TB,EI,A,TCA,KOUNTA,TCB,KOUNTB,MXKOUNT,KSTAR)
C
C     Rudolf Loeser, 1974 Nov 25
C---- Prints Color Temperature results.
C     !DASH
      save
C     !DASH
      real*8 A, EI, EMUI, EMUS, TB, TCA, TCB, W, WV, ZERO
      integer KOUNTA, KOUNTB, KSTAR, MXKOUNT, NF, NO
      character BLANK*1, TCAL*30, TCBL*30, VAR*15
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
C     !EJECT
      external ELATOS, ENCODED, TRESS, HI, BYE
C
      data EMUS /-1.D0/
C
      call HI ('BRAID')
C     !BEG
      if(EMUS.ne.EMUI) then
        EMUS = EMUI
C----   Print heading
        call ELATOS    (NO, EMUI)
      end if
C---- Print results
      VAR = BLANK
      call ENCODED     (WV, VAR(4:15), 12, 12, 1, NF)
      if(TCA.eq.ZERO) then
        write (NO,100) VAR,TB
  100   format(' ',A15,0PF15.0,5X,'No solution')
      else
        if(A.le.ZERO) then
          if(TCA.le.ZERO) then
            TCAL = 'TC > 100TB'
            TCBL = BLANK
          else
            call TRESS (TCAL, TCA, W, EI, KOUNTA, MXKOUNT,
     $                  KSTAR, 'TC')
            TCBL = BLANK
          end if
        else
          call TRESS   (TCAL, TCA, W, EI, KOUNTA, MXKOUNT,
     $                  KSTAR, 'TCA')
          if(TCB.le.ZERO) then
            TCBL = 'TCB < .01TB'
          else
            call TRESS (TCBL, TCB, W, EI, KOUNTB, MXKOUNT,
     $                  KSTAR, 'TCB')
          end if
        end if
        write (NO,101) VAR,TB,TCAL,TCBL
  101   format(' ',A15,0PF15.0,5X,A30,4X,A30)
      end if
C     !END
      call BYE ('BRAID')
C
      return
      end
