      subroutine BECARD
     $(N,ISB1,ISB2,RHOST,RHOSO,RHO)
C
C     Rudolf Loeser, 1986 Jul 24
C---- Combines the "static" and "Sobolev" rhos into one final table.
C     !DASH
      save
C     !DASH
      real*8 DX, RHO, RHOSO, RHOST, SO, ST, WSO, WST, X, X1, X2
      integer I, ISB1, ISB2, JSB1, JSB2, M, N
C     !DASH
      external  MOVE1, HI, BYE
      intrinsic min, max
C
C               RHOST(N), RHOSO(N), RHO(N)
      dimension RHOST(*), RHOSO(*), RHO(*)
C
      call HI ('BECARD')
C     !BEG
      JSB1 = max(1,ISB1)
      JSB2 = min(N,ISB2)
C
      call MOVE1 (RHOSO      ,JSB1,RHO      )
      M = N-JSB2+1
      call MOVE1 (RHOST(JSB2),M   ,RHO(JSB2))
C
      if((JSB2-JSB1).gt.1) then
        SO = RHOSO(JSB1)
        ST = RHOST(JSB2)
        X1 = JSB1
        X2 = JSB2
        DX = JSB2-JSB1
        do 100 I = (JSB1+1),(JSB2-1)
          X = I
          WSO = (X2-X)/DX
          WST = (X-X1)/DX
          RHO(I) = WSO*SO+WST*ST
  100   continue
      end if
C     !END
      call BYE ('BECARD')
C
      return
      end
