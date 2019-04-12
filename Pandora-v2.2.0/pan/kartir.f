      subroutine KARTIR
     $(NO,WTAB,K,MRR,DII,TOT,REM)
C
C     Rudolf Loeser, 1983 Jul 28
C---- Prints Integrated Disk Intensities, for CAESAR.
C     !DASH
      save
C     !DASH
      real*8 DII, WTAB
      integer I, IE, IS, J, K, MRR, NF, NO
      character REM*57, TOT*(*), W*12
C     !DASH
      external  LINER, ENCODED, MUSU, SHIM, TOTO, HI, BYE
      intrinsic min
C
C               DII(MRR,KM), WTAB(KM)
      dimension DII(MRR,*),  WTAB(*)
C
      call HI ('KARTIR')
C     !BEG
      if(NO.gt.0) then
        call MUSU        (NO,TOT,REM,'Disk ')
C
        IE = 0
  100   continue
          IS = IE+1
          IE = min(IE+10,MRR)
C
          call TOTO      (NO,'IID',IS,IE,1)
C
          do 102 J = 1,K
            call ENCODED (WTAB(J),W,12,12,1,NF)
            write (NO,101) J,W,(DII(I,J),I=IS,IE)
  101       format(' ',I3,1X,A12,1X,1P10E11.3)
            call SHIM    (J,5,NO)
  102     continue
C
        if(IE.lt.MRR) goto 100
      end if
C     !END
      call BYE ('KARTIR')
C
      return
      end
