      subroutine KLUNK
     $(Z,N,ZT1,SCON,ST1,XJNU,YT1,TAU,TMAX)
C
C     Rudolf Loeser, 1975 Apr 29
C---- Computes data for printing, for SHARI.
C     !DASH
      save
C     !DASH
      real*8 DT, ONE, SCON, SIG, ST1, TAU, TK, TMAX, TP, XJNU, YT1, Z,
     $       ZERO, ZT1
      integer K, N
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
      external NOTMORE, HI, BYE
C
C               TAU(N), Z(N), SCON(N), XJNU(N)
      dimension TAU(*), Z(*), SCON(*), XJNU(*)
C
      data SIG /-9.99D0/
C
      call HI ('KLUNK')
C     !BEG
      TMAX = TAU(N)
C
      if(TMAX.le.ONE) then
        ZT1 = Z(N)
        ST1 = SIG
        YT1 = SIG
      else
        call NOTMORE (TAU, N, ONE, K)
        TP = TAU(K+1)-ONE
        TK = ONE-TAU(K)
        DT = TAU(K+1)-TAU(K)
C
        ZT1 = (TP*Z(K)   +TK*Z(K+1)   )/DT
        ST1 = (TP*SCON(K)+TK*SCON(K+1))/DT
        YT1 = (TP*XJNU(K)+TK*XJNU(K+1))/DT
      end if
C     !END
      call BYE ('KLUNK')
C
      return
      end
