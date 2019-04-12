      subroutine FRAG
     $(TITLE,ARRAY,N,LINE,NO)
C
C     Rudolf Loeser, 1990 Nov 21
C---- Writes a line, with I10 format.
C     !DASH
      save
C     !DASH
      integer ARRAY, I, N, NO
      logical JAR
      character BLANK*1, LINE*120, TITLE*(*)
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, NAUGHTI, RIGHT, HI, BYE
C
C               ARRAY(N)
      dimension ARRAY(*)
C
      call HI ('FRAG')
C     !BEG
      if((N.lt.1).or.(N.gt.8)) then
        write (MSSLIN(1),100) N
  100   format('N =',I12,', which is not 1 through 8, inclusive.')
        call HALT  ('FRAG',1)
      end if
C
      call NAUGHTI (ARRAY,1,N,JAR)
      if(.not.JAR) then
        call RIGHT (TITLE,LINE(1:40),40)
        LINE(41:120) = BLANK
        write (LINE(51:120),101) (ARRAY(I),I=2,N)
  101   format(7I10)
        write (NO,102) LINE(2:120)
  102   format(' ',A)
      end if
C     !END
      call BYE ('FRAG')
C
      return
      end
