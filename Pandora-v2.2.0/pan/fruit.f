      subroutine FRUIT
     $(K,T,EPS)
C
C     Rudolf Loeser, 2006 Dec 28
C---- Computes EPS(T), the molecular equilibrium function for
C     OH (K=1), CH(K=2), or CO (K=3).
C     (This is version 3 of FRUIT.)
C     !DASH
      save
C     !DASH
      real*8 A, ARG, B, C, D, E, EPS, F, FAC, G, T, TLIM, TLN, ZERO
      integer K
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external HALT, HI, BYE
C
      dimension A(3), B(3), C(3), D(3), E(3), F(3), G(3)
C
      data A / 5.0968D+04, 4.0211D+04, 1.28710D+05 /
      data B / 4.5892D+01, 4.5508D+01, 4.90414D+01 /
      data C / 1.8453D-03, 1.6980D-03, 1.40306D-03 /
      data D / 5.0935D-07, 3.8478D-07, 2.66341D-07 /
      data E / 8.1825D-11, 5.8550D-11, 3.53827D-11 /
      data F / 5.0831D-15, 3.6153D-15, 2.65424D-15 /
      data G / 0.0000D+00, 0.0000D+00, 8.32385D-20 /
C
      data ZERO,FAC,TLIM /0.D0, 1.5D0, 1.D4/
C
      call HI ('FRUIT')
C     !BEG
      EPS = ZERO
C
      if((K.le.0).or.(K.ge.4)) then
        write (MSSLIN(1),100) K
  100   format('K =',I12,'; which is not 1, 2, or 3.')
        call HALT ('FRUIT', 1)
      end if
C
      if(T.le.ZERO) then
        write (MSSLIN(1),101) T
  101   format('T =',1PE16.8,'; cannot compute log.')
        call HALT ('FRUIT', 1)
      end if
C
      if(T.le.TLIM) then
        ARG = A(K)/T - B(K) + T*(C(K) - T*(D(K) - T*(E(K)
     $                                - T*(F(K) - T*G(K) ))))
        TLN = log(T)
        EPS = exp(ARG-FAC*TLN)
      end if
C     !END
      call BYE ('FRUIT')
C
      return
      end
