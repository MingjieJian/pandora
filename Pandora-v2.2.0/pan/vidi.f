      subroutine VIDI
     $(NO,WTAB,K,MRR,DI,EPC,TOT)
C
C     Rudolf Loeser, 1981 Aug 26
C---- Prints Disk Intensity Profiles, for CAESAR.
C     !DASH
      save
C     !DASH
      real*8 DI, EPC, WTAB
      integer I, IE, IS, J, K, MRR, NF, NO
      character TOT*(*), W*12
C     !DASH
      external  LINER, MULTER, ENCODED, SHIM, TOTO, HI, BYE
      intrinsic min
C
C               WTAB(KM), DI(MRR,KM), EPC(MRR)
      dimension WTAB(*),  DI(MRR,*),  EPC(*)
C
      call HI ('VIDI')
C     !BEG
      if(NO.gt.0) then
        call MULTER      (NO,TOT,'Disk ')
C
        IE = 0
  100   continue
          IS = IE+1
          IE = min(IE+10,MRR)
C
          call TOTO      (NO,' ID',IS,IE,1)
C
          do 102 J = 1,K
            call ENCODED (WTAB(J),W,12,12,1,NF)
            write (NO,101) J,W,(DI(I,J),I=IS,IE)
  101       format(' ',I3,1X,A12,1X,1P10E11.3)
            call SHIM    (J,5,NO)
  102     continue
C
          call LINER     (1,NO)
          write (NO,103) (EPC(I),I=IS,IE)
  103     format(' ',10X,' IID',3X,1P10E11.3)
C
        if(IE.lt.MRR) goto 100
      end if
C     !END
      call BYE ('VIDI')
C
      return
      end
