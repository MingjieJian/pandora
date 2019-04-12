      subroutine PLEAT
     $(FA,TCA,FB,TCB,W,EI,WP,EIP,TC,KOUNT,MXKOUNT,CRIT)
C
C     Rudolf Loeser, 1974 Nov 12
C---- Iterates to find a value of Color Temperature.
C     !DASH
      save
C     !DASH
      real*8 A, CRIT, EI, EIP, F, FA, FB, FF, HALF, TC, TCA, TCB, TCS,
     $       W, WP, ZERO
      integer IFLG, KOUNT, MXKOUNT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
C     !DASH
      external  COMPD, BRIDE, HI, BYE
      intrinsic max
C     !EJECT
C
      call HI ('PLEAT')
C     !BEG
      KOUNT = 0
C
      TC = max(TCA,TCB)
  100 continue
        TCS = TC
        TC  = HALF*(TCA+TCB)
        call COMPD (TCS,TC,CRIT,IFLG)
        if(IFLG.eq.0) then
          goto 101
        end if
        KOUNT = KOUNT+1
        if(KOUNT.gt.MXKOUNT) then
          goto 101
        end if
C
        call BRIDE (W,EI,WP,EIP,TC,A,FF,F)
        if(F.lt.ZERO) then
          if(FA.lt.ZERO) then
            FA  = F
            TCA = TC
            goto 100
C
          else if(FA.gt.ZERO) then
            FB  = F
            TCB = TC
            goto 100
C
          end if
        else if(F.gt.ZERO) then
          if(FB.lt.ZERO) then
            FA  = F
            TCA = TC
            goto 100
C
          else if(FB.gt.ZERO) then
            FB  = F
            TCB = TC
            goto 100
C
          end if
        end if
  101 continue
C     !END
      call BYE ('PLEAT')
C
      return
      end
