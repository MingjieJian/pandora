      subroutine SNAFU
     $(X,Y)
C
C     Rudolf Loeser, 2004 Mar 09
C---- Imposes machine range limits on a number.
C     (See also include-file ULTIMA.)
C     (See also SNUFFLE.)
C     !DASH
      save
C     !DASH
      real*8 X, Y, ZERO, ZZLARGE, ZZSMALL
C     !DASH
      external  HI, BYE
      intrinsic max, min
C
      data ZERO,ZZLARGE,ZZSMALL /0.D0, 1.D+300, 1.D-300/
C
      call HI ('SNAFU')
C     !BEG
      if(X.gt.ZERO) then
        Y =  ( max( min(( X),ZZLARGE) ,ZZSMALL) )
      else if(X.lt.ZERO) then
        Y = -( max( min((-X),ZZLARGE) ,ZZSMALL) )
      else
        Y = X
      end if
C     !END
      call BYE ('SNAFU')
C
      return
      end
