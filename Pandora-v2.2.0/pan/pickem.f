      subroutine PICKEM
     $(TAU,N,NB,XWGRTC, JS,JL)
C
C     Rudolf Loeser, 1996 Feb 28
C---- Picks 2 of the TAU sets, and returns their indices.
C     !DASH
      save
C     !DASH
      real*8 TAU, XWGRTC, ZERO
      integer IL, IS, J, JL, JS, K, L, LOOK, N, NB, jummy
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external LOOKSD, HI, BYE
C
C               TAU(N,NB)
      dimension TAU(N,*)
C     !EJECT
C
      call HI ('PICKEM')
C     !BEG
      JS = 0
      IS = N+1
      JL = 0
      IL = 0
C
      do 100 J = 1,NB
        call LOOKSD (TAU(1,J),N,ZERO,XWGRTC, K,jummy,LOOK)
C
        if(LOOK.eq.1) then
          L = K+1
        else if(LOOK.eq.4) then
          L = 1
        else
          L = 0
        end if
C
        if(L.gt.0) then
C
          if(L.lt.IS) then
            JS = J
            IS = L
          else if(L.eq.IS) then
            if(TAU(L,J).lt.TAU(L,JS)) then
              JS = J
              IS = L
            end if
          end if
C
          if(L.gt.IL) then
            JL = J
            IL = L
          else if(L.eq.IL) then
            if(TAU(L,J).gt.TAU(L,JL)) then
              JL = J
              IL = L
            end if
          end if
C
        end if
  100 continue
C     !END
      call BYE ('PICKEM')
C
      return
      end
