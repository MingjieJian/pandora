      subroutine SNUFFLE
     $(X,ZZLARGE,ZZSMALL,Y)
C
C     Rudolf Loeser, 2004 Mar 09
C---- Imposes machine range limits on a number.
C     (See also include-file ULTIMA.)
C     (See also SNAFU.)
C     !DASH
      save
C     !DASH
      real*8 X, Y, ZERO, ZZLARGE, ZZSMALL
C     !DASH
      external  HI, BYE
      intrinsic max, min
C
      data ZERO /0.D0/
C
      call HI ('SNUFFLE')
C     !BEG
      if(X.ge.ZERO) then
        Y =  ( max( min(( X),ZZLARGE) ,ZZSMALL) )
      else
        Y = -( max( min((-X),ZZLARGE) ,ZZSMALL) )
      end if
C     !END
      call BYE ('SNUFFLE')
C
      return
      end
