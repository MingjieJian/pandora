      subroutine ZIGAND
     $(N,Z,WS,SNU,X,D,DIDH,IMX)
C
C     Rudolf Loeser, 2000 May 18
C---- Computes smoothed dI/dh, for a given wavelength.
C     Also returns IMX, the index of the maximum value.
C     !DASH
      save
C     !DASH
      real*8 D, DIDH, DMX, FIVE, G, SNU, THREE, TWO, WS, X, Y, Z
      integer I, IMX, M, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 4),THREE )
      equivalence (DLIT( 6),FIVE  )
C     !DASH
C     !EJECT
      external DIVIDE, HI, BYE
C
C               Z(N), WS(N), SNU(N), X(N), D(N), DIDH(N)
      dimension Z(*), WS(*), SNU(*), X(*), D(*), DIDH(*)
C
      call HI ('ZIGAND')
C     !BEG
      M = N-1
C
      X(1) = Z(2)-Z(1)
      D(1) = WS(1)*SNU(1)
C
      do 100 I = 2,M
        X(I) = (Z(I+1)-Z(I-1))/TWO
        D(I) = WS(I)*SNU(I)
  100 continue
C
      X(N) = Z(N)-Z(M)
      D(N) = WS(N)*SNU(M)
C
      call DIVIDE   (D(1), X(1), DIDH(1))
      IMX = 1
      DMX = DIDH(1)
C
      do 101 I = 2,M
        G = (D(I-1)+THREE*D(I)+D(I+1))/FIVE
        Y = (X(I-1)+THREE*X(I)+X(I+1))/FIVE
        call DIVIDE (G, Y, DIDH(I))
        if(DIDH(I).gt.DMX) then
          IMX = I
          DMX = DIDH(I)
        end if
  101 continue
C
      call DIVIDE   (D(N), X(N), DIDH(N))
      if(DIDH(N).gt.DMX) then
        IMX = N
      end if
C     !END
      call BYE ('ZIGAND')
C
      return
      end
