      subroutine FROGG
     $(TITLE,ARRAY,N,LINE,IZERO,NO)
C
C     Rudolf Loeser, 2002 May 02
C---- Writes a line of ATMOSPHERE data, with variable format.
C     (See also FROG, which is for ATOM data ["7"].)
C
C---- Put IZERO = 0 to print floating zero as blank;
C               = 1                           "0".
C     !DASH
      save
C     !DASH
      real*8 ARRAY
      integer I, IZERO, K, N, NF, NO
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
      external HALT, NAUGHTD, RIGHT, ENCODED, HI, BYE
C
C               ARRAY(N)
      dimension ARRAY(*)
C     !EJECT
C
      call HI ('FROGG')
C     !BEG
      if((N.lt.1).or.(N.gt.8)) then
        write (MSSLIN(1),100) N
  100   format('N =',I12,' which is not 1 through 8, inclusive')
        call HALT      ('FROGG',1)
      end if
C
      call NAUGHTD     (ARRAY,1,N,JAR)
      if(.not.JAR) then
C
        call RIGHT     (TITLE,LINE(1:40),40)
        LINE(41:120) = BLANK
C
        K = 31
        do 101 I = 1,N
          K = K+10
          call ENCODED (ARRAY(I),LINE(K+1:K+9),9,7,IZERO,NF)
  101   continue
C
        write (NO,102) LINE(2:120)
  102   format(' ',A)
      end if
C     !END
      call BYE ('FROGG')
C
      return
      end
