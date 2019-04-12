      subroutine FRIG
     $(TITLE,ARRAY,N,LINE,IZERO,NO,IB,JH1,JH2,KODE,NN)
C
C     Rudolf Loeser, 1982 Jan 06
C---- This is a special version of "FROGGY", bracketing special values.
C     Sets up KODE when IB = 1, and returns KODE for use in subsequent
C     calls to FRIG.
C
C---- Put IZERO = 0 to print floating zero as blank;
C               = 1                           "0".
C
C     (This is version 2 of FRIG.)
C     !DASH
      save
C     !DASH
      real*8 ARRAY, CON
      integer I, IB, IZERO, JH1, JH2, K, KODE, L, N, NF, NN, NO
      logical JAR, KNST
      character BLANK*1, LBRAK*1, LINE*120, RBRAK*1, TITLE*(*)
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(47),LBRAK )
      equivalence (SYMBS(48),RBRAK )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, NAUGHTD, KONSTD, RIGHT, ENCODED, HI, BYE
C
C               ARRAY(N)
      dimension ARRAY(*)
C     !EJECT
C
      call HI ('FRIG')
C     !BEG
      if((N.lt.1).or.(N.gt.8)) then
        write (MSSLIN(1),100) N
  100   format('N =',I12,', which is not 1 through 8, inclusive.')
        call HALT      ('FRIG',1)
      end if
C
      if(IB.eq.1) then
        CON = ARRAY(1)
        call KONSTD    (ARRAY,1,NN,CON,KNST)
        if(KNST) then
          call RIGHT   (TITLE,LINE(1:40),40)
          write (NO,101) LINE(2:40),CON
  101     format(' ',A39,'     all =',1PE10.3)
          KODE = 1
        else
          KODE = 0
        end if
      end if
C
      if(KODE.eq.0) then
        call NAUGHTD     (ARRAY,1,N,JAR)
        if(.not.JAR) then
C
          call RIGHT     (TITLE,LINE(1:40),40)
          LINE(41:120) = BLANK
C
          L = IB-1
          K = 31
          do 102 I = 1,N
            K = K+10
            call ENCODED (ARRAY(I),LINE(K:K+8),9,6,IZERO,NF)
C
            LINE(K+9:K+9) = BLANK
            if((L+I).eq.JH1) then
              LINE(K  :K  ) = LBRAK
            else if((L+I).eq.JH2) then
              LINE(K+9:K+9) = RBRAK
            end if
C
  102     continue
          write (NO,103) LINE(2:120)
  103     format(' ',A)
        end if
C
      end if
C     !END
      call BYE ('FRIG')
C
      return
      end
