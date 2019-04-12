      subroutine FIDGET
     $(KILROY,N,NT,RHO,RHOO,RHNW,WEIT,WOLD)
C
C     Rudolf Loeser, 1981 Feb 13
C---- Computes a new current RHO, containing a proportion of
C     old RHO, intended to lead to positive B-ratio values.
C     (This is version 2 of FIDGET.)
C     !DASH
      save
C     !DASH
      real*8 ONE, RHNW, RHO, RHOO, TWO, W, WEIGHTS, WEIT, WINC, WOLD,
     $       ZERO, dummy
      integer KLIN, KMSS, KOUNT, MAXKNT, MODE, N, NNT, NT
      logical KILROY
      character qummy*8
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
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external HALT, MOVE1, WEITER, HI, BYE
C
C               RHO(N,NT), RHOO(N,NT), RHNW(N,NT), WEIT(N,NT)
      dimension RHO(*),    RHOO(*),    RHNW(*),    WEIT(*)
C
      parameter (MAXKNT=8)
      dimension WEIGHTS(MAXKNT)
 
      data KLIN,MODE,KMSS /0, 0, 0/
C     >>>>> KMSS = 1 might interfere with LILROY in
C                    FLIBBLE > RACE > LINEN; be sure to check!
C
      data WEIGHTS /
     $ .500000000000000D0, .875000000000000D0, .984375000000000D0,
     $ .999023437500000D0, .999969482421875D0, .999999523162842D0,
     $ .999999996274710D0, .999999999985448D0/
C     !EJECT
C
      call HI ('FIDGET')
C     !BEG
      if(KILROY) then
C----   Initialize
        W      = ZERO
        NNT    = N*NT
        WINC   = ONE
        KOUNT  = 0
        KILROY = .false.
      end if
C----
      KOUNT = KOUNT+1
      if(KOUNT.gt.MAXKNT) then
        write (MSSLIN(1),100)
  100   format('RHO fudging limit reached.')
        call HALT ('FIDGET', 1)
      end if
C
C---- Compute next weight W
      WINC = WINC/TWO
      W    = W+WINC
C---- Compute new "current RHO"
      call MOVE1  (RHO, NNT, RHNW)
      call WEITER (RHO, RHNW, RHOO, dummy, W, NNT, KLIN, MODE, KMSS,
     $             qummy, WEIT)
C---- Return value of "effective weight" of "old RHO"
      WOLD = WEIGHTS(KOUNT)
C     !END
      call BYE ('FIDGET')
C
      return
      end
