      subroutine BRIG
     $(TITLE,ARRAY,N,LINE,IZERO,NO)
C
C     Rudolf Loeser, 2006 Apr 21
C---- Special version of FROG (q.v.) for "A"s.
C     !DASH
      save
C     !DASH
      real*8 ARRAY, ZERO
      integer I, IZERO, K, L, N, NF, NO
      logical JAR
      character BLANK*1, LINE*120, TITLE*(*)
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
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external  HALT, NAUGHTD, RIGHT, ENCODED, HI, BYE
      intrinsic abs
C
C               ARRAY(N)
      dimension ARRAY(*)
C
      call HI ('BRIG')
C     !BEG
      if((N.lt.2).or.(N.gt.8)) then
        write (MSSLIN(1),100) N
  100   format('N =',I12,' which is not 2 through 8, inclusive')
        call HALT        ('BRIG', 1)
      end if
C
      if(IZERO.eq.0) then
        call NAUGHTD     (ARRAY, 1, N, JAR)
      else
        JAR = .false.
      end if
C
      if(.not.JAR) then
C
        call RIGHT       (TITLE, LINE(1:40), 40)
        LINE(41:120) = BLANK
C
        L = 0
        K = 31
        do 102 I = 1,N
          K = K+10
          if(ARRAY(I).ge.ZERO) then
            call ENCODED (ARRAY(I), LINE(K+1:K+9), 9, 7, L, NF)
            L = IZERO
          else
            write (LINE(K+1:K+9),101) abs(ARRAY(I))
  101       format('[',1PE7.1,']')
          end if
  102   continue
C
        write (NO,103) LINE(2:120)
  103   format(' ',A)
      end if
C     !END
      call BYE ('BRIG')
C
      return
      end
