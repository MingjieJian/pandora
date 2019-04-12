      subroutine CALLA
     $(M,Z,F,G,R,KBINN,KBOUT,A,B,C,ZZ,FF,GG)
C
C     Rudolf Loeser, 1988 Jun 16
C---- Computes the vectors A, B and C, for WALLA.
C     (This is version 3 of CALLA.)
C     !DASH
      save
C     !DASH
      real*8 A, B, C, DZIPIM, F, FF, G, GG, GIM, GIP, HALF, R, TFM, TFP,
     $       TWO, Z, ZERO, ZZ
      integer I, KBINN, KBOUT, M
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external MOVE1, CATRIN, HI, BYE
C
C               Z(M),    F(M),    G(M),   R(M), A(M), B(M), C(M),
      dimension Z(*),    F(*),    G(*),   R(*), A(*), B(*), C(*),
C
C               ZZ(M+2), FF(M+2), GG(M+2)
     $          ZZ(*),   FF(*),   GG(*)
C
      call HI ('CALLA')
C     !BEG
C---- Make augmented Z table (in cm!)
C     [such that ZZ(j+1) corresponds to Z(j)]
      call MOVE1    (Z,M,ZZ(2))
      ZZ(  1) = TWO*Z(1)-Z(  2)
      ZZ(M+2) = TWO*Z(M)-Z(M-1)
      call CATRIN   (ZZ,(M+2))
C
C---- Make augmented F and G tables
      call MOVE1    (F,M,FF(2))
      FF(  1) = F(1)
      FF(M+2) = F(M)
      call MOVE1    (G,M,GG(2))
      GG(  1) = G(1)
      GG(M+2) = G(M)
C     !EJECT
C---- Now, A, B, C
      do 100 I = 1,M
        DZIPIM = ZZ(I+2)-ZZ(I)
        GIM = (GG(I  )+GG(I+1))*HALF
        GIP = (GG(I+1)+GG(I+2))*HALF
        TFM = (FF(I  )+FF(I+1))/(ZZ(I+1)-ZZ(I  ))
        TFP = (FF(I+1)+FF(I+2))/(ZZ(I+2)-ZZ(I+1))
C
        if(I.le.1) then
          A(I) = ZERO
        else
          A(I) = -(TFM+GIM        )/DZIPIM
        end if
C
        B(I) =    (TFM-GIM+TFP+GIP)/DZIPIM+R(I)
C
        if(I.lt.M) then
          C(I) = -(TFP        -GIP)/DZIPIM
        else
          C(I) = ZERO
        end if
C
        if((I.eq.1).and.(KBINN.eq.1)) then
          C(1) = ZERO
          B(1) = R(1)
        else if((I.eq.M).and.(KBOUT.eq.1)) then
          A(M) = ZERO
          B(M) = R(M)
        end if
C
  100 continue
C     !END
      call BYE ('CALLA')
C
      return
      end
