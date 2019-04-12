      subroutine FROOT
     $(Y,H,B1)
C
C     Rudolf Loeser, 1992 Jan 03
C---- Computes the value of B1 corresponding to the given value of y.
C     !DASH
      save
C     !DASH
      real*8 B, B1, BK0, BK1, BL, BR, CRIT, F, F1, FL, FR, H, HALF, ONE,
     $       START, TWO, X, Y, ZK0, ZK1
      integer I, IR, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external  BESSEL, ENLIL, HI, BYE
      intrinsic abs, sign
C
      parameter (N=25)
      dimension B(N), F(N), BK0(N), BK1(N)
C
      data CRIT,START /1.D-4, 1.D-5/
C
      call HI ('FROOT')
C     !BEG
C---- Set up table for initial search
      X = HALF
      do 100 I = 1,N
        X = X*TWO
        B(I) = X*START
        call BESSEL   (B(I), BK0(I), BK1(I))
        F(I) = BK0(I)**2+BK1(I)**2-H
  100 continue
C     !EJECT
C---- Search for (the only permitted) change-of-sign
      IR = 0
      do 101 I = 2,N
        if(sign(ONE,F(I-1)).ne.sign(ONE,F(I))) then
          if(IR.eq.0) then
            IR = I
          else
            IR = -1
          end if
        end if
  101 continue
      if(IR.le.0) then
C       Print error message and abort
        call ENLIL    (B, BK0, BK1, F, N, Y, H, B1)
        goto 103
      end if
C
C---- Initialize halving search
      BL = B(IR-1)
      FL = F(IR-1)
      BR = B(IR)
      FR = F(IR)
C
C---- Now search (if necessary)
  102 continue
        B1 = HALF*(BL+BR)
        if((abs(BR-BL)/B1).gt.CRIT) then
          call BESSEL (B1, ZK0, ZK1)
          F1 = ZK0**2+ZK1**2-H
          if(sign(ONE,FL).eq.sign(ONE,F1)) then
            BL = B1
            FL = F1
          else
            BR = B1
            FR = F1
          end if
          go to 102
        end if
      continue
C
  103 continue
C     !END
      call BYE ('FROOT')
C
      return
      end
