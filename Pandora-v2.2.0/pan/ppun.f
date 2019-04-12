      subroutine PPUN
     $(A,F)
C
C     Rudolf Loeser, 2001 Jan 19
C---- Encodes a value of A, for PUN.
C     !DASH
      save
C     !DASH
      real*8 A, B, ZERO
      logical ESPION, XOK
      character D1*1, D2*1, E1*1, E2*1, F*16, X*1
C     !DASH
      external SNAFU, HI, BYE
C
      data E1, E2, D1, D2 /'E', 'e', 'D', 'd'/
C
      data ZERO,ESPION /0.D0, .true./
C
      call HI ('PPUN')
C     !BEG
      if(ESPION) then
C       Imposing the limit (EXM) enforced by NUDEAL
        call SNAFU (A, B)
      else
        B = A
      end if
C
      write (F,100) B
  100 format(1PE15.8)
C
      if(B.ne.ZERO) then
        X   = F(12:12)
        XOK = (X.eq.E1).or.(X.eq.E2).or.(X.eq.D1).or.(X.eq.D2)
        if(.not.XOK) then
          F(11:11) = E1
        end if
      end if
C     !END
      call BYE ('PPUN')
C
      return
      end
