      subroutine AMUR
     $(TAU,IMAX,N,FIN,TMS,WN,W)
C
C     Rudolf Loeser, 1985 Jan 03
C---- Computes WN = "Lambda-minus-one" operator by the "GR" method.
C     (See also PUR.)
C
C     TMS  is a threshhold value of TAU such that a parabolic fit to S
C          should be used for WN(i,i-1), WN(i,i), and WN(i,i+1),
C          whenver TAU(i,i) .ge. TMS.
C     FIN  should .eq. "true" for a finite slab, and should
C          .eq. "false" for a semi-infinite medium.
C     IMAX .le. N (in case the lower rows of the matrix are not needed).
C
C     (This is version 3 of AMUR.)
C     !DASH
      save
C     !DASH
      real*8 TAU, TMS, W, WN
      integer IDD, IEE, IMAX, IN, IPP, IRR, IS, ISS, MOX, N
      logical FIN
C     !DASH
      external USSURI, SHILKA, WGIVE, HI, BYE
C
      dimension W(*)
C
C               TAU(N,N), WN(N,N)
      dimension TAU(*),   WN(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),IEE   ),(IN( 2),ISS   ),(IN( 3),IRR   ),(IN( 4),IDD   ),
     $(IN( 5),IPP   )
C
      call HI ('AMUR')
C     !BEG
C     (Get, and allocate, W allotment)
      call USSURI (IN,IS,MOX,'AMUR',N)
C
      call SHILKA (N,IMAX,TAU,FIN,TMS,WN,W(IDD),W(IEE),W(ISS),W(IRR),
     $             W(IPP))
C
C     (Give back W allotment)
      call WGIVE  (W,'AMUR')
C     !END
      call BYE ('AMUR')
C
      return
      end
