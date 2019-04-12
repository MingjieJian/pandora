      subroutine FRESH
     $(J,JP,K,KP,ISO,WN,RC1213,CF,CA,PRNT,FVAL)
C
C     Rudolf Loeser, 1993 Mar 18
C---- Computes the f-value for the CO line (J -> JP, K -> KP),
C     for Carbon isotope C(12) or C(13) as specified by ISO,
C     according to
C
C     new data from G. Chackerian.
C     !DASH
      save
C     !DASH
      real*8 A, ABUN, CA, CF, EFK, EME, EMK, FA, FVAL, ONE, RC1213, TM,
     $       TWO, WN, XJ, XJP, XM, ZERO
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
      external  DUANE, DIEGO, ARATUS, FOLD, HI, BYE
      intrinsic abs, max
C
      call HI ('FRESH')
C     !BEG
      KD = KP-K
      JD = JP-J
C
      if((KD.eq.2).and.(ISO.eq.13)) then
        call FOLD     (J, JP, K, KP, ISO, WN, RC1213, CF, CA, PRNT,
     $                 FVAL)
      else
C
        if(JD.eq.-1) then
          M = -J
        else if(JD.eq.1) then
          M = J+1
        end if
C
        XM = abs(M)
        XJ = J
        TM = XM/(TWO*XJ+ONE)
C
        call DUANE    (K, KD, ISO,    EMK)
        call DIEGO    (K, KD, ISO, M, EFK)
        FVAL = CF*WN*TM*EMK*EFK
C     !EJECT
        if(PRNT) then
          call ARATUS ((ISO-11), RC1213, ABUN)
          EME = sqrt(max((EMK*EFK),ZERO))
          XJP = JP
          FA  = (TWO*XJ+ONE)/(TWO*XJP+ONE)
          A   = CA*(WN**2)*FA*FVAL
          write (*,102) J,JP,K,KP,WN,FVAL, CF,M,ISO,RC1213,ABUN,
     $                  TM,EMK,EFK, EME,CA,A
  102     format(' ',4X,'j=',I3,3X,'j''=',I3,3X,'v=',I3,3X,'v''=',I3,
     $              3X,'WN=',1PE22.15,'   *****   f=',E22.15/
     $           ' ',4X,'CF=',E22.15,3X,'m=',I4,3X,'isotope C',I2,3X,
     $              'C1213=',E10.3,3X,E16.10/
     $           ' ',4X,'mj-term=',E22.15,3X,'M(k)**2=',E22.15,3X,
     $               'F(k,m) term=',E22.15/
     $           ' ',4X,'ME=',E22.15,3X,'CA=',E22.15,3X,'A=',E22.15)
        end if
      end if
C     !END
      call BYE ('FRESH')
C
      return
      end
