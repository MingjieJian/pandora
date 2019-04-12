      subroutine LINTR
     $(X1,F1,X2,F2,MODE,X,F)
C     Rudolf Loeser, 1991 Jan 30.
C---- Linear Inter(Extra)polation,
C     using the two given points (X1,F1) and (X2,F2),
C     for real*4 operands.
C
C---- If MODE=1, then use F1 and F2 to compute F;
C     if MODE=2, then use log(F1) and log(F2) to compute F;
C     if MODE=3, then use logs of everything to compute F;
C     if MODE=4, then use log10(F1) and log10(F2) to compute F;
C     if MODE=5, then use log10s of everything to compute F.
C     !DASH
      save
C     !DASH
      real*4 F, F1, F2, FF, FF1, FF2, TEN, X, X1, X2, XX, XX1, XX2
      integer MODE
C     !DASH
      external ABORT
C
      data TEN /1.E1/
C     !EJECT
C
C     !BEG
      if(MODE.eq.1) then
        XX1 = X1
        XX2 = X2
        FF1 = F1
        FF2 = F2
        XX = X
      else if(MODE.eq.2) then
        XX1 = X1
        FF1 = log(F1)
        XX2 = X2
        FF2 = log(F2)
        XX = X
      else if(MODE.eq.3) then
        XX1 = log(X1)
        FF1 = log(F1)
        XX2 = log(X2)
        FF2 = log(F2)
        XX = log(X)
      else if(MODE.eq.4) then
        XX1 = X1
        FF1 = log10(F1)
        XX2 = X2
        FF2 = log10(F2)
        XX = X
      else if(MODE.eq.5) then
        XX1 = log10(X1)
        FF1 = log10(F1)
        XX2 = log10(X2)
        FF2 = log10(F2)
        XX = log10(X)
      else
        write (*,100) MODE
  100   format(' ','Error in LINTR: MODE =',I12)
        call ABORT
      end if
      FF = FF1+(FF2-FF1)*((XX-XX1)/(XX2-XX1))
      if((MODE.eq.2).or.(MODE.eq.3)) then
        F = exp(FF)
      else if((MODE.eq.4).or.(MODE.eq.5)) then
        F = TEN**FF
      else
        F = FF
      end if
C     !END
C
      return
      end
