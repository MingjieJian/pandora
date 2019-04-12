      subroutine FROG
     $(TITLE,ARRAY,N,LINE,IZERO,NO)
C
C     Rudolf Loeser, 1980 Dec 27
C---- Writes a line of ATOM data, with variable format.
C     (See also FROGG, which is for ATMOSPHERE data ["8"].)
C
C---- Put IZERO = 0 to print floating zero as blank;
C               = 1                           "0".
C
C---- Note: ARRAY(1) is used only for TER;
C     TER = 0. should always print as blank.
C
C     (This is version 3 of FROG.)
C     !DASH
      save
C     !DASH
      real*8 ARRAY
      integer I, IZERO, K, L, N, NF, NO
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
C     !EJECT
      external HALT, NAUGHTD, RIGHT, ENCODED, HI, BYE
C
C               ARRAY(N)
      dimension ARRAY(*)
C
      call HI ('FROG')
C     !BEG
      if((N.lt.2).or.(N.gt.8)) then
        write (MSSLIN(1),100) N
  100   format('N =',I12,' which is not 2 through 8, inclusive')
        call HALT      ('FROG', 1)
      end if
C
      if(IZERO.eq.0) then
        call NAUGHTD   (ARRAY, 1, N, JAR)
      else
        JAR = .false.
      end if
C
      if(.not.JAR) then
C
        call RIGHT     (TITLE, LINE(1:40), 40)
        LINE(41:120) = BLANK
C
        L = 0
        K = 31
        do 101 I = 1,N
          K = K+10
          call ENCODED (ARRAY(I), LINE(K+1:K+9), 9, 7, L, NF)
          L = IZERO
  101   continue
C
        write (NO,102) LINE(2:120)
  102   format(' ',A)
      end if
C     !END
      call BYE ('FROG')
C
      return
      end
