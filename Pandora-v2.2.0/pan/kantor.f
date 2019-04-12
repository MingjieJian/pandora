      subroutine KANTOR
     $(KZAUG,KZANX,IVLSW,MN1,Z,F,G,R,S,H,ZXH,M,ZA,FA,GA,RA,SA,HA,ZXHA,
     $ VEC)
C
C     Rudolf Loeser, 2004 Jan 28
C---- Sets up expanded tables, for CARROT.
C     !DASH
      save
C     !DASH
      real*8 F, FA, G, GA, H, HA, R, RA, S, SA, VEC, Z, ZA, ZXH, ZXHA
      integer IVLSW, KSUM, KZANX, KZAUG, M, MN1
C     !DASH
      external IRRSUM, INDARRI, MOVE1, RAGGED, CRIMP, HI, BYE
C
C               J = N+MXTAP
C
C               KZAUG(N), KZANX(N), Z(N), F(N), G(N), R(N), S(N), H(N),
      dimension KZAUG(*), KZANX(*), Z(*), F(*), G(*), R(*), S(*), H(*),
C
C               ZXH(N), ZA(J), FA(J), GA(J), RA(J), SA(J), HA(J),
     $          ZXH(*), ZA(*), FA(*), GA(*), RA(*), SA(*), HA(*),
C
C               ZXHA(J), VEC(J)
     $          ZXHA(*), VEC(*)
C
      call HI ('KANTOR')
C     !BEG
      call IRRSUM    (KZAUG, MN1, KSUM)
      if(KSUM.eq.0) then
C----   No expansion needed
        M = MN1
        call INDARRI (KZANX, 1, 1, MN1)
        call MOVE1   (Z,   MN1, ZA  )
        call MOVE1   (F,   MN1, FA  )
        call MOVE1   (G,   MN1, GA  )
        call MOVE1   (R,   MN1, RA  )
        call MOVE1   (S,   MN1, SA  )
        call MOVE1   (H,   MN1, HA  )
        call MOVE1   (ZXH, MN1, ZXHA)
      else
C----   Construct expanded table
        call RAGGED  (Z, MN1, KZAUG, KZANX, ZA, M)
C----   Interpolate
        call CRIMP   (Z, R,   MN1, ZA, RA,   M, VEC)
        call CRIMP   (Z, S,   MN1, ZA, SA,   M, VEC)
        if(IVLSW.eq.0) then
          call CRIMP (Z, F,   MN1, ZA, FA,   M, VEC)
          call CRIMP (Z, G,   MN1, ZA, GA,   M, VEC)
        else
          call CRIMP (Z, H,   MN1, ZA, HA,   M, VEC)
          call CRIMP (Z, ZXH, MN1, ZA, ZXHA, M, VEC)
        end if
      end if
C     !END
      call BYE ('KANTOR')
C
      return
      end
