      subroutine ANOR
     $(NO,WTAB,K,MRR,INT1D,TAU1D,TOT)
C
C     Rudolf Loeser, 1983 May 20
C---- Prints Disk depths of formation, for CAESAR.
C     !DASH
      save
C     !DASH
      real*8 TAU1D, TSUB, WTAB
      integer I, IE, INT1D, IS, ISUB, J, K, KNT, MRR, NF, NO
      character LINE*11, TOT*(*), W*12
C     !DASH
      external  ELDOL, MOHUCA, ENCODED, LINER, SHIM, TOTO, HI, BYE
      intrinsic min
C
C               WTAB(KM), INT1D(MRR,KM), TAU1D(MRR,KM)
      dimension WTAB(*),  INT1D(MRR,*),  TAU1D(MRR,*)
C
      dimension LINE(10), ISUB(10), TSUB(10)
C
      call HI ('ANOR')
C     !BEG
      if(NO.gt.0) then
        call MOHUCA      (NO,TOT,'Disk ')
C
        IE = 0
  100   continue
          IS = IE+1
          IE = min(IE+10,MRR)
C
          call TOTO      (NO,' ID',IS,IE,1)
C
          do 103 J = 1,K
            call ENCODED (WTAB(J),W,12,12,1,NF)
C
            KNT = 0
            do 101 I = IS,IE,+1
              KNT = KNT+1
              ISUB(KNT) = INT1D(I,J)
              TSUB(KNT) = TAU1D(I,J)
  101       continue
C
            call ELDOL   (ISUB,TSUB,LINE,KNT)
            write (NO,102) J,W,(LINE(I),I=1,KNT)
  102       format(' ',I3,1X,A12,1X,10A11)
            call SHIM    (J,5,NO)
  103     continue
C
        if(IE.lt.MRR) goto 100
      end if
C     !END
      call BYE ('ANOR')
C
      return
      end
