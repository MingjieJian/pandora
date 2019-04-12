      subroutine ODI
     $(NO,WTAB,K,MRR,DFC,TOT)
C
C     Rudolf Loeser, 1983 May 20
C---- Prints Disk Flux contributions, for CAESAR.
C     !DASH
      save
C     !DASH
      real*8 DFC, WTAB
      integer I, IE, IS, J, K, MRR, NF, NO
      character TOT*(*), W*12
C     !DASH
      external  LINER, MESET, ENCODED, SHIM, TOTO, HI, BYE
      intrinsic min
C
C               WTAB(KM), DFC(MRR,KM)
      dimension WTAB(*),  DFC(MRR,*)
C
      call HI ('ODI')
C     !BEG
      if(NO.gt.0) then
        call MESET       (NO,TOT,'Disk ')
C
        IE = 0
  100   continue
          IS = IE+1
          IE = min(IE+10,MRR)
C
          call TOTO      (NO,' FD',IS,IE,1)
C
          do 102 J = 1,K
            call ENCODED (WTAB(J),W,12,12,1,NF)
            write (NO,101) J,W,(DFC(I,J),I=IS,IE)
  101       format(' ',I3,1X,A12,1X,1P10E11.3)
            call SHIM    (J,5,NO)
  102     continue
C
        if(IE.lt.MRR) goto 100
      end if
C     !END
      call BYE ('ODI')
C
      return
      end
