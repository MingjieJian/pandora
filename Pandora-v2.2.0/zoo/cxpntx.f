      subroutine CXPNTX
     $(X,N,EI,EX,KODE)
C     Rudolf Loeser, 1981 Feb 27
C---- Decides whether or not to do a regular calculation,
C     for CEXPINT.
C     Returns with KODE=.true. if yes; =.false. if no.
C     (This is version 2 of CXPNTX.)
C     !DASH
      save
C     !DASH
      real*8 EI, EX, ONE, X, XN, ZERO
      integer N
      logical KODE
C     !DASH
      data ZERO, ONE /0.D0, 1.D0/
C
C     !BEG
      KODE = .true.
      if(X.eq.ZERO) then
        KODE = .false.
        EX   = ONE
        if(N.le.1) then
          EI = ZERO
        else
          XN = N-1
          EI = ONE/XN
        end if
      end if
C     !END
C
      return
      end
