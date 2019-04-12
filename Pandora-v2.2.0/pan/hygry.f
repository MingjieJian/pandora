      subroutine HYGRY
     $(X,XPBL,N,NTE,IRFNC,TER,TE,RZM,ZME,AEL,ZRN,XNC,ZHEL)
C
C     Rudolf Loeser, 1994 May 05
C     Revised RL/SGK Apr 10 2014 
C---- Computes default values of XNC and IRFNC.
C     !DASH
      save
C     !DASH
      real*8 AEL, RZM, TE, TER, X, XNC, XPBL, ZHEL, ZME, ZRN
      integer IRFNC, N, NTE
      logical ZXNC
C     !DASH
      external FRAGA, ABSALON, ZORNIG, NAUGHTD, ZERO1, HI, BYE
C
      dimension X(*)
C
C               XNC(N), RZM(N), ZME(N), AEL(N), ZHEL(N), TE(N),
      dimension XNC(*), RZM(*), ZME(*), AEL(*), ZHEL(*), TE(*),
C
C               XPBL(Lenpbl), TER(NTE), ZRN(N)
     $          XPBL(*),      TER(*),   ZRN(*)
C
      call HI ('HYGRY')
C     !BEG
      call NAUGHTD   (XNC, 1, N, ZXNC)
C
      if(ZXNC) then
        call ZERO1   (ZHEL, N)
        call ZORNIG  (N, ZME, RZM, AEL, ZHEL, ZRN)
        call FRAGA   (X, XPBL)
      end if
C
      if(IRFNC.le.0) then
        call ABSALON (N, NTE, IRFNC, TER, TE)
      end if
C     !END
      call BYE ('HYGRY')
C
      return
      end
