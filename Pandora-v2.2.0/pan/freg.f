      subroutine FREG
     $(TITLE,ARRAY,N,LINE,NO,PRINT)
C
C     Rudolf Loeser, 2002 Apr 30
C---- Writes a line, with variable format.
C     Special case of FROG: prints "zero", but prints blank for "one".
C     !DASH
      save
C     !DASH
      real*8 ARRAY, ONE
      integer I, IZERO, K, N, NF, NO
      logical JAR, PRINT
      character BLANK*1, LINE*120, TITLE*(*)
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
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
      external HALT, KONSTD, RIGHT, ENCODED, HI, BYE
C
C               ARRAY(N)
      dimension ARRAY(*)
C
      data IZERO /1/
C     !EJECT
C
      call HI ('FREG')
C     !BEG
      if((N.lt.1).or.(N.gt.8)) then
        write (MSSLIN(1),100) N
  100   format('N =',I12,' which is not 1 through 8, inclusive')
        call HALT        ('FREG',1)
      end if
C
      call KONSTD        (ARRAY(2),1,(N-1),ONE,JAR)
      PRINT = .not.JAR
      if(PRINT) then
C
        call RIGHT       (TITLE,LINE(1:40),40)
        LINE(41:120) = BLANK
C
        K = 41
        do 101 I = 2,N
          K = K+10
          if(ARRAY(I).eq.ONE) then
            LINE(K+1:K+9) = BLANK
          else
            call ENCODED (ARRAY(I),LINE(K+1:K+9),9,7,IZERO,NF)
          end if
  101   continue
C
        write (NO,102) LINE(2:120)
  102   format(' ',A)
      end if
C     !END
      call BYE ('FREG')
C
      return
      end
