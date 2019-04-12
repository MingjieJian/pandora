      subroutine FOLD
     $(J,JP,K,KP,ISO,WN,RC1213,CF,CA,PRNT,FVAL)
C
C     Rudolf Loeser, 1992 Sep 11
C---- Computes the f-value for the CO line (J -> JP, K -> KP),
C     for Carbon isotope C(12) or C(13) as specified by ISO,
C     according to
C
C     C. Chackerian, Jr. and R. H. Tipping (1983),
C     J.Mol.Spectrosc., 99, 431-449.
C     !DASH
      save
C     !DASH
      real*8 A, ABUN, CA, CF, EM, EM1, EM2, EME, EMF, F, FA, FK, FKD,
     $       FKP, FVAL, G, H, ONE, RC1213, TK, TM, TWO, WN, XJ, XJP, XM,
     $       ZERO
      integer ISO, J, JD, JP, K, KD, KP, M
      logical PRNT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
C     !EJECT
      external  FACTIN, COE, COG, COR, ARATUS, HI, BYE
      intrinsic abs, max
C
      data EM1,EM2 /1.084D-1, 6.6D-3/
C
      call HI ('FOLD')
C     !BEG
      KD = KP-K
      if(KD.eq.0) then
        EM = ZERO
      else if(KD.eq.1) then
        EM = EM1
      else
        EM = EM2
      end if
C
      JD = JP-J
      if(JD.eq.-1) then
        M = -J
      else if(JD.eq.1) then
        M = J+1
      end if
C
      XM = abs(M)
      XJ = J
C
      TM = XM/(TWO*XJ+ONE)
C
      if(KD.gt.0) then
        G = ZERO
        call FACTIN (KP, FKP)
        call FACTIN (K,  FK)
        call FACTIN (KD, FKD)
        TK = FKP/(FK*FKD)
C
        call COE    (K, KD,    ISO, H)
        call COG    (K, KD, M, ISO, F)
C
        FVAL = CF*WN*(EM**2)*TM*TK*H*F
C
      else
        TK = ZERO
        H  = ZERO
        F  = ZERO
C
        call COR    (K, M, ISO, G)
C
        FVAL = CF*WN*TM*G
      end if
C     !EJECT
      if(PRNT) then
C
        call ARATUS  ((ISO-11), RC1213, ABUN)
        if(KD.gt.0) then
          EMF = sqrt(max((TK*H*F),ZERO))
          EME = EM*EMF
        else
          EME = sqrt(max(G,ZERO))
        end if
        XJP = JP
        FA  = (TWO*XJ+ONE)/(TWO*XJP+ONE)
        A   = CA*(WN**2)*FA*FVAL
C
        write (*,100) J,JP,K,KP,WN,FVAL, CF,EM,M,ISO,RC1213,ABUN,
     $                TM,TK,H,F, EME,CA,A,G
  100   format(' ',4X,'j=',I3,3X,'j''=',I3,3X,'v=',I3,3X,'v''=',I3,
     $            3X,'WN=',1PE22.15,'   *****   f=',E22.15/
     $         ' ',4X,'CF=',E22.15,3X,'M=',E22.15,3X,'m=',I4,
     $             3X,'isotope C',I2,3X,'C1213=',E10.3,3X,E16.10/
     $         ' ',4X,'mj-term=',E22.15,3X,'v-term=',E22.15,3X,
     $             3X,'H=',E22.15,3X,'F=',E22.15/
     $         ' ',4X,'ME=',E22.15,3X,'CA=',E22.15,3X,'A=',E22.15,
     $             3X,'G=',E22.15)
      end if
C     !END
      call BYE ('FOLD')
C
      return
      end
