      subroutine VENI
     $(NO,WTAB,K,N,SI,EPC,EPS,TOT)
C
C     Rudolf Loeser, 1981 Aug 26
C---- Prints Shell Intensity Profiles, for CAESAR.
C     !DASH
      save
C     !DASH
      real*8 EPC, EPS, SI, WTAB
      integer I, IE, IS, J, K, N, NF, NO
      character TOT*(*), W*12
C     !DASH
      external  LINER, MULTER, ENCODED, SHIM, TOTO, HI, BYE
      intrinsic max
C
C               WTAB(KM), SI(N,KM), EPC(N), EPS(N)
      dimension WTAB(*),  SI(N,*),  EPC(*), EPS(*)
C
      call HI ('VENI')
C     !BEG
      if(NO.gt.0) then
        call MULTER      (NO,TOT,'Shell')
C
        IE = N+1
  100   continue
          IS = IE-1
          IE = max(IE-10,1)
C
          call TOTO      (NO,' IS',IS,IE,-1)
C
          do 102 J = 1,K
            call ENCODED (WTAB(J),W,12,12,1,NF)
            write (NO,101) J,W,(SI(I,J),I=IS,IE,-1)
  101       format(' ',I3,1X,A12,1X,1P10E11.3)
            call SHIM    (J,5,NO)
  102     continue
C
          call LINER     (1,NO)
          write (NO,103) ' IIS',(EPC(I),I=IS,IE,-1)
  103     format(' ',10X,A4,3X,1P10E11.3)
          write (NO,103) 'IICM',(EPS(I),I=IS,IE,-1)
C
        if(IE.gt.1) goto 100
      end if
C     !END
      call BYE ('VENI')
C
      return
      end
