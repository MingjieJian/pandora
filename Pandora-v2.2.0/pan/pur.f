      subroutine PUR
     $(TAU,IMAX,N,FIN,TMS,WH,W)
C
C     Rudolf Loeser, 1989 Jun 28
C---- Computes WH = "Phi" operator by the "GR" method.
C     (See also AMUR.)
C
C     TMS  is a threshhold value of TAU such that a parabolic fit
C          to S should be used for WH(i,i-1), WH(i,i), and WH(i,i+1)
C          whenever TAU(i,i) .ge. TMS.
C     FIN  should .eq. "true" for a finite slab, and should
C          .eq. "false" for a semi-infinite medium.
C     IMAX .le. N (in case the lower rows of the matrix are not needed).
C     !DASH
      save
C     !DASH
      real*8 TAU, TMS, W, WH
      integer IDD, IEE, IMAX, IN, IRR, IS, ISS, MOX, N
      logical FIN
C     !DASH
      external CHI, TAZ, WGIVE, HI, BYE
C
      dimension W(*)
C
C               TAU(N,N), WH(N,N)
      dimension TAU(*),   WH(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IEE   ),(IN( 2),ISS   ),(IN( 3),IRR   ),(IN( 4),IDD   )
C
      call HI ('PUR')
C     !BEG
C     (Get, and allocate, W allotment)
      call CHI   (IN,IS,MOX,'PUR',N)
C
      call TAZ   (N,IMAX,TAU,FIN,TMS,WH,W(IDD),W(IEE),W(ISS),W(IRR))
C
C     (Give back W allotment)
      call WGIVE (W,'PUR')
C     !END
      call BYE ('PUR')
C
      return
      end
