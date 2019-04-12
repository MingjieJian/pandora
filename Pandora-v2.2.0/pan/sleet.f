      subroutine SLEET
     $(N,MF,ML,W,WS,XINT,IWS)
C
C     Rudolf Loeser, 1973 Dec 05
C---- Normalizes and rounds for OOBLECK.
C     !DASH
      save
C     !DASH
      real*8 F, HALF, ONE, RX1, RX2, SWS, W, WS, XINT
      integer I, IWS, J, MF, ML, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  DIVIDE, HI, BYE
      intrinsic mod
C
C               W(N,NW), WS(N,NW), IWS(N,NW), XINT(NW)
      dimension W(N,*),  WS(N,*),  IWS(N,*),  XINT(*)
C
      data RX1,RX2 /1.D2, 9.95D1/
C
      call HI ('SLEET')
C     !BEG
      do 101 J = MF,ML
        call DIVIDE (RX1,XINT(J),F)
C
        do 100 I = 1,N
          SWS = F*W(I,J)*WS(I,J)
          if(SWS.ge.RX2) then
            IWS(I,J) = RX1
          else if(SWS.lt.HALF) then
            IWS(I,J) = 0
          else
            IWS(I,J) = SWS
            if(mod(SWS,ONE).ge.HALF) then
              IWS(I,J) = IWS(I,J)+1
            end if
          end if
  100   continue
C
  101 continue
C     !END
      call BYE ('SLEET')
C
      return
      end
