      subroutine DONAR
     $(IU,IL,TEMP,CE)
C
C     Rudolf Loeser, 1990 Nov 19
C---- Computes default values of CE(2,1) for Hydrogen.
C
C     Scholz, Walters, Burke, and Scott (1990), MNRAS, 242, 692.
C     !DASH
      save
C     !DASH
      real*8 B, C, CE, CON58, FAC, G1S2P, G1S2S, RTE, SIGMA, TE, TEMP,
     $       TLIM1, TLIM2, X, XE, XL, ZERO
      integer IL, IU
      logical INOK, TEOK
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  RIGEL, HI, BYE
      intrinsic min, max
C
      dimension B(8), C(6)
C
      data B / 4.5168D-2,  2.8056D+1,  7.2945D+0,  2.4805D-1,
     $         1.0044D-1, -1.1143D-2, -1.3432D-3,  3.7570D-4/
      data C / 3.6177D-1,  1.3891D+0,  5.0866D-1, -3.8011D-1,
     $         1.0158D-1, -1.0072D-2/
      data FAC,TLIM1,TLIM2 /4.315D-6, 2.D3, 5.D5/
C
      call HI ('DONAR')
C     !BEG
      CE = ZERO
C
      INOK = (IU.eq.2).and.(IL.eq.1)
      TEOK = TEMP.gt.ZERO
      if(INOK.and.TEOK) then
        TE = min(max(TEMP,TLIM1),TLIM2)
        call RIGEL (58,CON58)
        X  = TE/CON58
        XL = log(B(2)*X)
        XE = exp(-B(3)*X)
        G1S2S = B(1)*XL*XE+B(4)+X*(B(5)+X*(B(6)+X*(B(7)+X*B(8))))
        G1S2P = C(1)+X*(C(2)+X*(C(3)+X*(C(4)+X*(C(5)+X*C(6)))))
        SIGMA = G1S2S+G1S2P
        RTE   = sqrt(TE)
C
        CE = (FAC/RTE)*SIGMA
      end if
C     !END
      call BYE ('DONAR')
C
      return
      end
