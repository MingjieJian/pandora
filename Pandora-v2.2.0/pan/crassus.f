      subroutine CRASSUS
     $(NO,WTAB,K,N,SII,TOT,REM)
C
C     Rudolf Loeser, 1983 Jul 28
C---- Prints integrated shell intensities, for CAESAR.
C     !DASH
      save
C     !DASH
      real*8 SII, WTAB
      integer I, IE, IS, J, K, N, NF, NO
      character REM*57, TOT*(*), W*12
C     !DASH
      external  LINER, ENCODED, MUSU, SHIM, TOTO, HI, BYE
      intrinsic max
C
C               SII(N,KM), WTAB(KM)
      dimension SII(N,*),  WTAB(*)
C
      call HI ('CRASSUS')
C     !BEG
      if(NO.gt.0) then
        call MUSU        (NO,TOT,REM,'Shell')
C
        IE = N+1
  100   continue
          IS = IE-1
          IE = max(IE-10,1)
C
          call TOTO      (NO,'IIS',IS,IE,-1)
C
          do 102 J = 1,K
            call ENCODED (WTAB(J),W,12,12,1,NF)
            write (NO,101) J,W,(SII(I,J),I=IS,IE,-1)
  101       format(' ',I3,1X,A12,1X,1P10E11.3)
            call SHIM    (J,5,NO)
  102     continue
C
        if(IE.gt.1) goto 100
      end if
C     !END
      call BYE ('CRASSUS')
C
      return
      end
