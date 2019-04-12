      subroutine GRACIAS
     $(K,A,V,VP,MORD,W,GII)
C
C     Rudolf Loeser, 2005 Feb 10
C---- Computes a table of values of GII(A,VP),
C     for a table of values of V, of length K.
C     MORD specifies the degree of the wing approximation;
C     it may take on one of the values: 0, 1, or 2.
C     W is a scratch array.
C     !DASH
      save
C     !DASH
      real*8 A, FIVE, FOUR, GII, HALF, ONE, PI, ROOTPI, TWO, V, VP, W,
     $       ZERO
      integer K, M, MORD
      logical REV
C     !COM
      common /APXGII/ PI,ROOTPI,ZERO,HALF,ONE,TWO,FOUR,FIVE
C     !DASH
      external  NEGATE, REVERSD, APGII
      intrinsic max,min
C
C               GII(K), V(K), W(K)
      dimension GII(*), V(*), W(*)
C
      data PI,ROOTPI /3.141592653589793D0, 1.772453850905516D0/
C
      data ZERO,HALF, ONE /0.D0, 5.D-1, 1.D0/
      data  TWO,FOUR,FIVE /2.D0, 4.D0,  5.D0/
C
C     !BEG
      M = min(max(MORD,0),2)
C
      REV = VP.lt.ZERO
      if(REV) then
        VP = -VP
        call NEGATE  (V, K)
        call REVERSD (V, 1, K)
      end if
C
      call APGII     (K, A, V, VP, M, W, GII)
C
      if(REV) then
        call REVERSD (GII, 1, K)
      end if
C     !END
C
      return
      end
      subroutine APGII
     $(N,A,V,VP,M,W,GII)
C
C     Rudolf Loeser, 2004 Nov 17
C
C---- Approximates GII(v,v') = RII(v,v')/PHI(v'), v' .ge. 0, and
C     the table of v if increasing order.
C     M = 0, 1, or 2 to specify order of wing approximation;
C     W is a scratch array of length N.
C     Adapted from the code of
C
C     P .   G o u t t e b r o z e ,
C
C     A&A 1986, 160, 195-98.
C---- Modified to recognize A=0 as a special case, 2005 Feb 18.
C     !DASH
      save
C     !DASH
      real*8 A, DV, DV1, DV2, FIVE, FOUR, GII, HALF, ONE, PCORE, PI,
     $       PWING, ROOTPI, TRM, TWO, V, VMAX, VMIN, VP, VP2, W, ZERO
      integer I, I1, I2, M, N
C     !COM
      common /APXGII/ PI,ROOTPI,ZERO,HALF,ONE,TWO,FOUR,FIVE
C     !DASH
      external  GIIWING, GIICORE
      intrinsic abs
C
C               V(N), W(N), GII(N)
      dimension V(*), W(*), GII(*)
C
C     !BEG
      VMIN = -FOUR
      if(VP.gt.FOUR) then
        VMIN = VP-FIVE
      end if
      VMAX = VP+FIVE
C
      GII(1) = ZERO
      I1  = 1
      DV1 = abs(V(1)-VMIN)
      I2  = 1
      DV2 = abs(V(1)-VMAX)
      do 100 I = 2,N
        GII(I) = ZERO
        DV = abs(V(I)-VMIN)
        if(DV.lt.DV1) then
          I1  = I
          DV1 = DV
        end if
        DV = abs(V(I)-VMAX)
        if(DV.lt.DV2) then
          I2  = I
          DV2 = DV
        end if
  100 continue
C     !EJECT
      if(A.eq.ZERO) then
        call GIICORE   (1, N, V, VP, GII)
      else
C
        if(VP.gt.FOUR) then
C----     Line Wing Region
          call GIIWING (I1, I2, V, VP, M, GII)
C
        else if(VP.ge.TWO) then
C----     Transition Region
          call GIIWING (I1, I2, V, VP, M, W  )
          call GIICORE (I1, I2, V, VP,    GII)
C
          VP2 = VP**2
          TRM = exp(-VP2)
          PWING = ONE/(ROOTPI*(A+VP2/A)*TRM+ONE)
          PCORE = ONE-PWING
C
          do 101 I = I1,I2
            GII(I) = PCORE*GII(I)+PWING*W(I)
  101     continue
C
        else
C----     Line Core Region
          call GIICORE (I1, I2, V, VP, GII)
        end if
C
      end if
C     !END
C
      return
      end
      subroutine GIICORE
     $(I1,I2,V,VP,GII)
C
C     Rudolf Loeser, 2004 Nov 17
C---- Computes core values of GII, for APGII.
C     !DASH
      save
C     !DASH
      real*8 FIVE, FOUR, GII, GZ, GZVP, HALF, ONE, P1, PI, ROOTPI, TERM,
     $       TWO, V, V2, VP, VP2, ZERO
      integer I, I1, I2
      logical KILROY
C     !COM
      common /APXGII/ PI,ROOTPI,ZERO,HALF,ONE,TWO,FOUR,FIVE
C     !DASH
      intrinsic abs
C
C               V(N), GII(N)
      dimension V(*), GII(*)
C
      data KILROY /.true./
C
C     !BEG
      if(KILROY) then
        KILROY = .false.
        P1 = FOUR/PI
      end if
C
      VP2  = VP**2
      TERM = sqrt(VP2+P1)
      GZVP = ONE/(abs(VP)+TERM)
      do 100 I = I1,I2
        if(abs(V(I)).le.VP) then
          GII(I) = GZVP
        else
          V2   = V(I)**2
          TERM = sqrt(V2+P1)
          GZ   = ONE/(abs(V(I))+TERM)
          TERM = exp(VP2-V2)
          GII(I) = GZ*TERM
        end if
  100 continue
C     !END
C
      return
      end
      subroutine GIIWING
     $(I1,I2,V,VP,M,GII)
C
C     Rudolf Loeser, 2004 Nov 17
C---- Computes wing values of GII, for APGII.
C     !DASH
      save
C     !DASH
      real*8 AUX, AUX2, C1, C2, C3, FIVE, FOUR, GIGI, GII, GZAUX, HALF,
     $       ONE, P1, P2, PI, ROOTPI, TERM, TWO, V, VP, VRAT, ZERO
      integer I, I1, I2, M
      logical KILROY
C     !COM
      common /APXGII/ PI,ROOTPI,ZERO,HALF,ONE,TWO,FOUR,FIVE
C     !DASH
      intrinsic abs
C
C               V(N), GII(N)
      dimension V(*), GII(*)
C
      data C1,C2,C3 /2.75D0, -2.5D0, 0.75D0/
C
      data KILROY /.true./
C
C     !BEG
      if(KILROY) then
        KILROY = .false.
        P1 = FOUR/PI
        P2 = ONE/ROOTPI
      end if
C
      do 103 I = I1,I2
        VRAT  = V(I)/VP
        AUX   = abs(HALF*(V(I)-VP))
        AUX2  = AUX**2
        TERM  = sqrt(AUX2+P1)
        GZAUX = ONE/(abs(AUX)+TERM)
        TERM  = exp(-AUX2)
        GIGI  = P2*TERM*(ONE-TWO*AUX*GZAUX)
C
        goto (102, 101, 100), (M+1)
  100   continue
          GIGI = GIGI*(C1+(C2+C3*VRAT)*VRAT)
  101   continue
          GIGI = GIGI*(TWO-VRAT)
  102   continue
C
        GII(I) = GIGI
  103 continue
C     !END
C
      return
      end
