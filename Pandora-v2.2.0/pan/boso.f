      subroutine BOSO
     $(NO,WTAB,K,N,INT1S,TAU1S,TOT)
C
C     Rudolf Loeser, 1983 May 20
C---- Prints Shell depths of formation, for CAESAR.
C     !DASH
      save
C     !DASH
      real*8 TAU1S, TSUB, WTAB
      integer I, IE, INT1S, IS, ISUB, J, K, KNT, N, NF, NO
      character LINE*11, TOT*(*), W*12
C     !DASH
      external  ELDOL, MOHUCA, ENCODED, LINER, SHIM, TOTO, HI, BYE
      intrinsic max
C
C               WTAB(KM), INT1S(N,KM), TAU1S(N,KM)
      dimension WTAB(*),  INT1S(N,*),  TAU1S(N,*)
C
      dimension LINE(10), ISUB(10), TSUB(10)
C
      call HI ('BOSO')
C     !BEG
      if(NO.gt.0) then
        call MOHUCA      (NO,TOT,'Shell')
        IE = N+1
  100   continue
          IS = IE-1
          IE = max(IE-10,1)
C
          call TOTO      (NO,' IS',IS,IE,-1)
C
          do 103 J = 1,K
            call ENCODED (WTAB(J),W,12,12,1,NF)
C
            KNT = 0
            do 101 I = IS,IE,-1
              KNT = KNT+1
              ISUB(KNT) = INT1S(I,J)
              TSUB(KNT) = TAU1S(I,J)
  101       continue
C
            call ELDOL   (ISUB,TSUB,LINE,KNT)
            write(NO,102) J,W,(LINE(I),I=1,KNT)
  102       format(' ',I3,1X,A12,1X,10A11)
            call SHIM    (J,5,NO)
C
  103     continue
C
        if(IE.gt.1) goto 100
      end if
C     !END
      call BYE ('BOSO')
C
      return
      end
