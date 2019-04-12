      subroutine WILIWAW
     $(NO,I,N,LINE,Z,KOELS,MODE)
C
C     Rudolf Loeser, 2001 DEC 10
C---- Prints depth-of-formation information.
C     !DASH
      save
C     !DASH
      real*8 Z, ZERO, ZSV
      integer I, KNT, KOELS, MODE, N, NO
      logical OMIT
      character BLANK*1, LINE*(*)
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
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
      external HALT, HI, BYE
C     !EJECT
C
      call HI ('WILIWAW')
C     !BEG
      if((MODE.lt.1).or.(MODE.gt.2)) then
        write (MSSLIN(1),100) MODE
  100   format('MODE =',I12,', which is not 1 or 2.')
        call HALT ('WILIWAW', 1)
      end if
C
      if((KOELS.lt.0).or.(KOELS.gt.1)) then
        write (MSSLIN(1),101) KOELS
  101   format('KOELS =',I12,', which is not 0 or 1.')
        call HALT ('WILIWAW', 1)
      end if
C
      if(KOELS.eq.1) then
        if(MODE.eq.1) then
          write (NO,102) Z,I,LINE
  102     format(' ',1PE11.4,I4,A)
        else
          write (NO,103) I,LINE
  103     format(' ',I4,' - ',A)
        end if
        goto 106
      end if
C
C---- Get here only if KOELS=0
      if(I.eq.1) then
C----   Start of loop over depths - initialize
        KNT = 0
        ZSV = ZERO
      end if
C
      OMIT = (I.ne.1).and.(I.ne.N).and.(LINE.eq.BLANK)
C     !EJECT
      if(OMIT) then
C----   Blank lines (except the first and the last line) are not
C       printed. They are counted; the count restarts for each new
C       batch of blank lines after the last preceding non-blank line.
        KNT = KNT+1
        ZSV = Z
      else
C
C----   Print this line - but first check what went before
        if(KNT.eq.1) then
C----     There was only one omitted blank line - print it anyway
          if(MODE.eq.1) then
            write (NO,102) ZSV,(I-1),BLANK
          else
            write (NO,103) (I-1),BLANK
          end if
        else if(KNT.gt.1) then
C----     There were several omitted blank lines - print marker
          if(MODE.eq.1) then
            write (NO,104)
  104       format(' ',11X,'****')
          else
            write (NO,105)
  105       format(' ','****')
          end if
        end if
C
C----   Now, finally, print this line
        if(MODE.eq.1) then
          write (NO,102) Z,I,LINE
        else
          write (NO,103) I,LINE
        end if
        KNT = 0
      end if
C
  106 continue
C     !END
      call BYE ('WILIWAW')
C
      return
      end
