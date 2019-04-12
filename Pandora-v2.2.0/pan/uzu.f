      subroutine UZU
     $(TE,N,K)
C
C     Rudolf Loeser, 1981 Feb 02
C---- Determines index, K, of reference depth, for VANUA.
C     !DASH
      save
C     !DASH
      real*8 CRIT, DI, DP, DTIS, ONE, TE, ZERO
      integer I, K, MN, MX, N
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
      external  HI, BYE
      intrinsic sign, abs
C
C               TE(N)
      dimension TE(*)
C
      data CRIT /4.D4/
C     !EJECT
C
      call HI ('UZU')
C     !BEG
      K = N
C
      if(K.gt.1) then
        K = 0
        DTIS = TE(2)-TE(1)
        if(DTIS.lt.ZERO) then
          if(TE(1).le.CRIT) then
            K = 1
          end if
        else
          if(TE(1).ge.CRIT) then
            K = 1
          end if
        end if
C
        if(K.eq.0) then
          MN = 1
          MX = 1
          do 100 I = 2,N
            if(TE(I).lt.TE(MN)) then
              MN = I
            end if
            if(TE(I).gt.TE(MX)) then
              MX = I
            end if
            DI = TE(I)-CRIT
            DP = TE(I-1)-CRIT
            if(sign(ONE,DI).ne.sign(ONE,DP)) then
              if(abs(DI).lt.abs(DP)) then
                K = I
              else
                K = I-1
              end if
              goto 101
            end if
  100     continue
          if(DTIS.lt.ZERO) then
            K = MN
          else
            K = MX
          end if
  101     continue
        end if
      end if
C     !END
      call BYE ('UZU')
C
      return
      end
