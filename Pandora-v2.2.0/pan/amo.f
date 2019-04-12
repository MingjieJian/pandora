      subroutine AMO
     $(NO,WTAB,K,N,SFC,TOT)
C
C     Rudolf Loeser, 1983 May 20
C---- Prints Shell Flux contributions, for CAESAR.
C     !DASH
      save
C     !DASH
      real*8 SFC, WTAB
      integer I, IE, IS, J, K, N, NF, NO
      character TOT*(*), W*12
C     !DASH
      external  LINER, MESET, ENCODED, SHIM, TOTO, HI, BYE
      intrinsic max
C
C               WTAB(KM), SFC(N,KM)
      dimension WTAB(*),  SFC(N,*)
C
      call HI ('AMO')
C     !BEG
      if(NO.gt.0) then
        call MESET       (NO,TOT,'Shell')
C
        IE = N+1
  100   continue
          IS = IE-1
          IE = max(IE-10,1)
C
          call TOTO      (NO,' FS',IS,IE,-1)
C
          do 102 J = 1,K
            call ENCODED (WTAB(J),W,12,12,1,NF)
            write (NO,101) J,W,(SFC(I,J),I=IS,IE,-1)
  101       format(' ',I3,1X,A12,1X,1P10E11.3)
            call SHIM    (J,5,NO)
  102     continue
C
        if(IE.gt.1) goto 100
      end if
C     !END
      call BYE ('AMO')
C
      return
      end
