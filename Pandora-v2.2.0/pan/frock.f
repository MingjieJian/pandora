      subroutine FROCK
     $(TITLE,ARRAY,N,LINE,IZERO,NO)
C
C     Rudolf Loeser, 2006 Mar 29
C---- Special version of FROG (q.v.): for wavenumbers.
C     !DASH
      save
C     !DASH
      real*8 ARRAY
      integer I, IZERO, K, L, M, MAX, N, NF, NO
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
      data MAX /1000000000/
C
      call HI ('FROCK')
C     !BEG
      if((N.lt.2).or.(N.gt.8)) then
        write (MSSLIN(1),100) N
  100   format('N =',I12,' which is not 2 through 8, inclusive')
        call HALT        ('FROCK', 1)
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
          M = ARRAY(I)
          if((M.lt.MAX).and.(M.ne.0)) then
            write (LINE(K+1:K+9),101) M
  101       format(I9)
          else
            call ENCODED (ARRAY(I), LINE(K+1:K+9), 9, 7, L, NF)
            L = IZERO
          end if
  102   continue
C
        write (NO,103) LINE(2:120)
  103   format(' ',A)
      end if
C     !END
      call BYE ('FROCK')
C
      return
      end
