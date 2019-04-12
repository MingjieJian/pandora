      subroutine ASSUR
     $(X,D,M,EA)
C
C     Rudolf Loeser, 1981 Mar 26
C---- Computes the EA[m](X,D) functions, for 3 .le. m .le. 5.
C     Returns EA .eq. 0 when X .gt. CRIT or X .le. 0.
C     !DASH
      save
C     !DASH
      real*8 D, DELTA, DS, E1, EA, EX, ONE, ORDNL, P, PR, PX, RF, RS,
     $       SUM, T, X, XS, ZERO
      integer I, KM, KS, M
      logical DNE, XNE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
      external  EXPINT, HI, BYE
      intrinsic abs
C
      dimension ORDNL(30), RF(30), P(30)
C
      data KM,KS /30, 0/
      data ORDNL,RF,P /30*0.D0, 30*0.D0, 30*0.D0/
      data XS,DS,RS,E1,EX,DELTA /0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 1.D-16/
C     !EJECT
C
      call HI ('ASSUR')
C     !BEG
      if(ORDNL(1).eq.ZERO) then
C----   Initialize tables
        ORDNL(1) = ONE
        RF(1)    = ONE
        do 100 I = 2,KM
          ORDNL(I) = ORDNL(I-1)+ONE
          RF(I)    = RF(I-1)/ORDNL(I)
  100   continue
      end if
C
      if((X.le.ZERO).or.(X.gt.ZLNLARG)) then
        EA = ZERO
      else
C
        XNE = X.ne.XS
        DNE = D.ne.DS
        if(XNE) then
C----     Compute exponential terms
          XS = X
          call EXPINT (1,XS,E1,EX)
        end if
C
        if(XNE.or.DNE) then
C----     Compute basic terms of sum, and limit
          DS = D
          RS = DS/XS
          PR = ONE
          PX = ONE
          T  = ONE
          do 101 I = 1,KM
            KS = I
            PR = PR*RS
            PX = PX*XS
            T  = T*ORDNL(I)+PX
            P(I) = T*PR
            if(abs(P(I)).lt.DELTA) then
              goto 102
            end if
  101     continue
  102     continue
        end if
C
C----   Compute sum
        SUM = RF(M)
        do 103 I = 1,KS
          SUM = SUM+P(I)*RF(I+M)
  103   continue
C
        EA = (D**(M-1))*(E1*RF(M-1)+RS*EX*SUM)
      end if
C     !END
      call BYE ('ASSUR')
C
      return
      end
