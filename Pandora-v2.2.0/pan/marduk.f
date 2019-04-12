      subroutine MARDUK
     $(X,M,ES)
C
C     Rudolf Loeser, 1981 Mar 25
C---- Computes the ES[m](X) functions, for 2 .le. m .le. 5.
C     !DASH
      save
C     !DASH
      real*8 CONT, DELTA, ES, ONE, P, RF, SUM, W, X, XLOG, XN, XP, ZERO
      integer I, M
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external  HI, BYE
      intrinsic abs
C
      dimension RF(11), P(7), W(5)
C
      data RF,P,XLOG,DELTA /11*0.D0, 7*0.D0, 0.D0, 1.D-12/
      data W /0.D0, 4.22784335098467D-1, 4.61392167549234D-1,
     $              2.09352944738633D-1, 6.27549028513250D-2/
C
      call HI ('MARDUK')
C     !BEG
      if(RF(1).eq.ZERO) then
C----   Initialize table of reciprocal factorials
        RF(1) = ONE
        do 100 I = 2,11
          XN    = I
          RF(I) = RF(I-1)/XN
  100   continue
      end if
C
      if(X.eq.ZERO) then
        ES = ZERO
      else
C
        if(X.ne.P(1)) then
C----     Set up power table, and log
          XP   = X
          P(1) = X
          do 101 I = 2,7
            XP   = -XP*X
            P(I) =  XP/ORD(I+1)
  101     continue
          XLOG = log(X)
        end if
C
C----   Compute sum
        SUM = W(M)-XLOG*RF(M-1)
        do 102 I = 1,7
          CONT = P(I)*RF(I+(M-1))
          SUM  = SUM+CONT
          if(abs(CONT/SUM).lt.DELTA) then
            goto 103
          end if
  102   continue
C
  103   continue
C----   Compute ES[m]
        ES = ((-X)**(M-1))*SUM
C
      end if
C     !END
      call BYE ('MARDUK')
C
      return
      end
