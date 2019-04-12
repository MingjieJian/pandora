      subroutine FRUG
     $(TITLE,ARRAY,N,LINE,NO)
C
C     Rudolf Loeser, 1980 Dec 27
C---- Writes a line, with A10 format.
C     (This is version 2 of FRUG.)
C     !DASH
      save
C     !DASH
      integer I, N, NO
      logical JAR
      character ARRAY*(*), BLANK*1, LINE*120, TITLE*(*)
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
      external HALT, KONSTC, RIGHT, HI, BYE
C
C               ARRAY(N)
      dimension ARRAY(*)
C
      call HI ('FRUG')
C     !BEG
      if((N.lt.1).or.(N.gt.8)) then
        write (MSSLIN(1),100) N
  100   format('N =',I12,', which is not 1 through 8, inclusive.')
        call HALT  ('FRUG',1)
      end if
C
      call KONSTC  (ARRAY,1,N,'          ',JAR)
C
      if(.not.JAR) then
        call RIGHT (TITLE,LINE(1:40),40)
        LINE(41:120) = BLANK
        write (LINE(41:120),101) (ARRAY(I),I=1,N)
  101   format(8A10)
        write (NO,102) LINE(2:120)
  102   format(' ',A)
      end if
C     !END
      call BYE ('FRUG')
C
      return
      end
