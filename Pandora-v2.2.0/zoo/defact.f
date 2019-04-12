      subroutine DEFACT
     $(X,N,FACTOR,OK)
C     Rudolf Loeser, 1990 Dec 12
C---- Scales the N x N matrix in X, and
C     returns the log of the scale FACTOR,
C     in a manner suitable for determinant calculations.
C     Also returns OK = .true. if things seem allright; but
C     = .false. if not, in which case neither X nor FACTOR
C     are useable.
C     !DASH
      save
C     !DASH
      real*8 AX, EF, F, FACTOR, HALF, TMN, TMNL, TMX, TMXL, X, ZERO,
     $       ZZLARGE
      integer I, J, N
      logical OK
C     !DASH
      intrinsic abs
C
      dimension X(N,N)
C
      data ZERO, HALF, ZZLARGE /0.D0, 5.D-1, 1.D+300/
C
C     !BEG
      FACTOR = ZERO
      OK = .true.
C---- Scale rows
      do 102 I = 1,N
        TMN = +ZZLARGE
        TMX = -ZZLARGE
        do 100 J = 1,N
          AX = abs(X(I,J))
          if(AX.lt.TMN) then
            TMN = AX
          end if
          if(AX.gt.TMX) then
            TMX = AX
          end if
  100   continue
        OK = TMN.gt.ZERO
        if(.not.OK) goto 106
        TMNL = log(TMN)
        TMXL = log(TMX)
C
        F = HALF*(TMNL+TMXL)
        FACTOR = FACTOR+F
        EF = exp(-F)
        OK = EF.gt.ZERO
        if(.not.OK) goto 106
        do 101 J = 1,N
          X(I,J) = EF*X(I,J)
  101   continue
  102 continue
C     !EJECT
C---- Scale columns
      do 105 J = 1,N
        TMN = +ZZLARGE
        TMX = -ZZLARGE
        do 103 I = 1,N
          AX = abs(X(I,J))
          if(AX.lt.TMN) then
            TMN = AX
          end if
          if(AX.gt.TMX) then
            TMX = AX
          end if
  103   continue
        OK = TMN.gt.ZERO
        if(.not.OK) goto 106
        TMNL = log(TMN)
        TMXL = log(TMX)
C
        F = HALF*(TMNL+TMXL)
        FACTOR = FACTOR+F
        EF = exp(-F)
        OK = EF.gt.ZERO
        if(.not.OK) goto 106
        do 104 I = 1,N
          X(I,J) = EF*X(I,J)
  104   continue
  105 continue
C
  106 continue
C     !END
C
      return
      end
