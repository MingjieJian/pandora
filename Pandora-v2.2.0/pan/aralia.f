      subroutine ARALIA
     $(NO,L,ONAME,MLAB)
C
C     Rudolf Loeser, 1989 Apr 17
C---- Prints for FOP via ALYSSUM.
C     !DASH
      save
C     !DASH
      integer KOPT, L, NO
      character BLANK*1, LINE*128, MLAB*4, ONAME*8
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external HI, BYE
C
      data KOPT /-1/
C
      call HI ('ARALIA')
C     !BEG
      if(MLAB.eq.'ZZZZ') then
        if(KOPT.ge.0) then
          write (NO,101) LINE
        end if
      else
C
        if(L.eq.1) then
          LINE = BLANK
          KOPT = -16
        end if
C
        KOPT = KOPT+16
        write (LINE(KOPT+1:KOPT+16),100) ONAME,MLAB
  100   format(' ',A10,1X,A4)
C
        if(KOPT.ge.112) then
          write (NO,101) LINE
  101     format(A128)
          LINE = BLANK
          KOPT = -16
        end if
C
      end if
C     !END
      call BYE ('ARALIA')
C
      return
      end
