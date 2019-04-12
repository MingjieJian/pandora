      subroutine AIM
     $(TRYO,RO,TRY,R,NTRY2,B,N,XLDT,NDT,APD,TW,VEC,RB,RJ,MDTRY2,DDT,
     $ DUMP)
C
C     Rudolf Loeser, 1973 Oct 25
C---- Zeroes in on the answer, for SINEW.
C     !DASH
      save
C     !DASH
      real*8 APD, B, DDT, DV, HALF, ONE, R, R1, R2, RB, RJ, RO, T1, T2,
     $       TRY, TRYO, TW, VEC, XLDT
      integer MDTRY2, N, NDT, NTRY2
      logical DUMP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  PLONK, FOX, DIVIDE, DIDDLE, HI, BYE
      intrinsic abs, sign, min
C
C               XLDT(NDT), APD(NDT), TW(NDT), B(N,NDT), VEC(NDT)
      dimension XLDT(*),   APD(*),   TW(*),   B(*),     VEC(*)
C     !EJECT
C
      call HI ('AIM')
C     !BEG
      NTRY2 = 0
      if(TRYO.ne.TRY) then
C
        if(TRYO.lt.TRY) then
          T1 = TRYO
          R1 = RO
          T2 = TRY
          R2 = R
        else
          T1 = TRY
          R1 = R
          T2 = TRYO
          R2 = RO
        end if
C
C----   Compute new RB
  100   continue
C
          TRY = HALF*(T1+T2)
          call PLONK    (NDT, XLDT, TRY, B)
          call FOX      (NDT, B, APD, VEC, TW, RB)
          DV = min(RB,RJ)
          call DIVIDE   ((RB-RJ), DV, R)
          if(DUMP) then
            call DIDDLE (TRY, RB, R, NTRY2, 'NTRY2')
          end if
C
C----     Check result
          if(abs(R).ge.DDT) then
            NTRY2 = NTRY2+1
C
            if(NTRY2.lt.MDTRY2) then
C----         Try again
              if(sign(ONE,R).ne.sign(ONE,R1)) then
                T2 = TRY
                R2 = R
              else
                T1 = TRY
                R1 = R
              end if
              goto 100
            end if
C
          end if
C
      end if
C     !END
      call BYE ('AIM')
C
      return
      end
