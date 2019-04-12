      subroutine NIDABA
     $(N,WNR,WNZ,W)
C
C     Rudolf Loeser, 1981 Oct 28
C---- Shuffles the Ray-WN-matrix, WNR, to make the Z-WN-matrix, WNZ.
C     !DASH
      save
C     !DASH
      real*8 W, WNR, WNZ
      integer IN, IS, IW1, IW2, IW3, M5, MOX, N, N3
C     !DASH
      external LAGASH, TIAMAT, KINGU, PASUKA, TAKINI, WGIVE, HI, BYE
C
      dimension W(*)
C
C               WNR(2*N+5,2*N+5), WNZ(N,N)
      dimension WNR(*),           WNZ(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IW1   ),(IN( 2),IW2   ),(IN( 3),IW3   )
C
      call HI ('NIDABA')
C     !BEG
      M5 = 2*N+5
      N3 = N+3
C     (Get, and allocate, W allotment)
      call LAGASH (IN,IS,MOX,'NIDABA',M5,N3,N)
C
C---- Transform   WNR(2N+5,2N+5)   to   W1(N+3,2N+5).
      call TIAMAT (M5,N3,WNR,W(IW1))
C
C---- Transform   W1(N+3,2N+5)     to   W2(N+3,N+3).
      call KINGU  (M5,N3,W(IW1),W(IW2))
C
C---- Transform   W2(N+3,N+3)      to   W3(N,N+3).
      call PASUKA (N3,N,W(IW2),W(IW3))
C
C---- Transform   W3(N,N+3)        to   WNZ(N,N).
      call TAKINI (N3,N,W(IW3),WNZ)
C
C     (Give back W allotment)
      call WGIVE  (W,'NIDABA')
C     !END
      call BYE ('NIDABA')
C
      return
      end
