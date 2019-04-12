      subroutine MULATOR
     $(LU,N,M,Z,HND,PND,W,IW)
C
C     Rudolf Loeser, 2007 Mar 28
C---- Supervises the plot of "ND/HND".
C     !DASH
      save
C     !DASH
      real*8 HND, PMAX, PND, W, Z
      integer IN, IPNN, IS, IW, IXX, LU, M, MOX, N
C     !DASH
      external MULLAH, COCKLE, CALICUT, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               PND(N,M), HND(N), Z(N)
      dimension PND(N,*), HND(*), Z(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1), IPNN  ),(IN( 2),IXX   )
C
      call HI ('MULATOR')
C     !BEG
      if((LU.gt.0).and.(M.gt.5)) then
C       (Get, and allocate, W allotment)
        call MULLAH  (IN, IS, MOX, 'MULATOR')
C
        call COCKLE  (N, M, HND, PND, W(IPNN), PMAX)
        call CALICUT (LU, N, M, W(IXX), W(IPNN), PMAX)
C
C       (Give back W allotment)
        call WGIVE   (W, 'MULATOR')
      end if
C     !END
      call BYE ('MULATOR')
C
      return
      end
