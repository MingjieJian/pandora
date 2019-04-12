      subroutine FUSSY
     $(JU,JL,IB,IE,KIJ,QAR,M)
C
C     Rudolf Loeser, 1980 Dec 28
C---- Identifies special transition types, for HOLME.
C     !DASH
      save
C     !DASH
      integer I, IB, IE, JL, JU, JUL, K, KIJ, LIM, M
      character NON*10, QAR*10, TIT*10
C     !DASH
      external INDXUL, HI, BYE
C
      parameter (LIM=5)
      dimension TIT(LIM)
C
C               JU(MUL), JL(MUL), KIJ(MUL), QAR(16)
      dimension JU(*),   JL(*),   KIJ(*),   QAR(*)
C
      data TIT /' Radiative', ' (Passive)', '    (Thin)',
     $          '(2-photon)', '   (Thick)'/
      data NON /'  nonsense'/
C
      call HI ('FUSSY')
C     !BEG
      M = 1
      do 100 I = IB,IE
        M = M+1
C
        call INDXUL (JU(I),JL(I),JUL)
        K = KIJ(JUL)
        if((K.ge.1).and.(K.le.LIM)) then
          QAR(M) = TIT(K)
        else if(K.ne.0) then
          QAR(M) = NON
        end if
C
  100 continue
C     !END
      call BYE ('FUSSY')
C
      return
      end
