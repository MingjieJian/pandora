      subroutine HODRO
     $(INK,XINK,FINK,IPNT,VEC)
C
C     Rudolf Loeser, 1979 Dec 07
C---- Sorts incident radiation data into ascending order.
C     !DASH
      save
C     !DASH
      real*8 FINK, VEC, XINK
      integer INK, IPNT
C     !DASH
      external SORT, ORDERD, HI, BYE
C
C               XINK(INK), FINK(INK), IPNT(INK), VEC(INK)
      dimension XINK(*),   FINK(*),   IPNT(*),   VEC(*)
C
      call HI ('HODRO')
C     !BEG
      if(INK.gt.1) then
        call SORT   (XINK, INK, IPNT, 'Incident radiation frequencies')
        call ORDERD (FINK, IPNT, INK, VEC)
      end if
C     !END
      call BYE ('HODRO')
C
      return
      end
