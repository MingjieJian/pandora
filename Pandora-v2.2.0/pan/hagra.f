      subroutine HAGRA
     $(XLHM,AHM,YHM,MHM,IPNT,VEC)
C
C     Rudolf Loeser, 1984 Aug 16
C---- Makes sure that XLHM is in ascending sorted order.
C     !DASH
      save
C     !DASH
      real*8 AHM, DELTA, VEC, XLHM, YHM
      integer IPNT, KSORT, MHM, jummy
C     !COM
      common   /JUDGD/ DELTA
C     !DASH
      external JUDGED, SORTDD, SORT, ORDERD, HI, BYE
C
C               XLHM(MHM), AHM(MHM), YHM(MHM), IPNT(MHM), VEC(MHM)
      dimension XLHM(*),   AHM(*),   YHM(*),   IPNT(*),   VEC(*)
C
      data DELTA /1.D-8/
C
      call HI ('HAGRA')
C     !BEG
      call SORTDD   (XLHM, MHM, JUDGED, KSORT, jummy)
      if(KSORT.ne.1) then
        call SORT   (XLHM, MHM, IPNT, 'H- wavelengths')
        call ORDERD (AHM, IPNT, MHM, VEC)
        call ORDERD (YHM, IPNT, MHM, VEC)
      end if
C     !END
      call BYE ('HAGRA')
C
      return
      end
