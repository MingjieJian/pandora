      subroutine READY
     $(NTRY1,MDTRY1,DDT,RB,TDUSTO,B,N,XLDT,NDT,APD,TW,VEC,RJ,TRYO,RO,
     $ TRY,R,KODE,DUMP)
C     Rudolf Loeser, 1973 Oct 73
C---- Finds a bracketing interval for SINEW.
C     !DASH
      save
C     !DASH
      real*8 APD, B, DDT, DV, HALF, ONE, R, RB, RJ, RO, TDUSTO, TRY,
     $       TRYO, TW, TWO, VEC, XLDT, ZERO
      integer KODE, MDTRY1, N, NDT, NTRY1
      logical DUMP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external  PLONK, FOX, DIDDLE, DIVIDE, HI, BYE
      intrinsic abs, sign, min
C
C               XLDT(NDT), APD(NDT), TW(NDT), B(NDT), VEC(NDT)
      dimension XLDT(*),   APD(*),   TW(*),   B(*),   VEC(*)
C     !EJECT
C
      call HI ('READY')
C     !BEG
      KODE  = 0
      NTRY1 = 0
      R     = ZERO
      RO    = ZERO
      TRY   = TDUSTO
      TRYO  = TDUSTO
C
C---- Compute new RB
  100 continue
C
        call PLONK    (NDT, XLDT, TRY, B)
        call FOX      (NDT, B, APD, VEC, TW, RB)
C----   Check the result
        RO = R
        DV = min(RB,RJ)
        call DIVIDE   ((RB-RJ), DV, R)
        if(DUMP) then
          call DIDDLE (TRY, RB, R, NTRY1, 'NTRY1')
        end if
        if(abs(R).ge.DDT) then
          if((NTRY1.gt.0).and.(sign(ONE,RO).ne.sign(ONE,R))) goto 101
          NTRY1 = NTRY1+1
          if(NTRY1.ge.MDTRY1) goto 101
C
C----     Try again
          if(R.gt.ZERO) then
            TRYO = TRY
            TRY  = HALF*TRY
            goto 100
C
          else if(R.lt.ZERO) then
            TRYO = TRY
            TRY  = TWO*TRY
            goto 100
          end if
C
        else
C----     Bull's eye
          KODE = 1
        end if
      continue
C
  101 continue
C     !END
      call BYE ('READY')
C
      return
      end
