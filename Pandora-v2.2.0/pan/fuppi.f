      subroutine FUPPI
     $(HNI,HNIR,LIMP,N,NO)
C
C     Rudolf Loeser, 1981 Jun 16
C---- Prints arrays for H.S.E. debugging.
C     !DASH
      save
C     !DASH
      real*8 HNI, HNIR
      integer IE, IS, LIMP, N, NO
C     !DASH
      external  HUPPI, HI, BYE
      intrinsic min
C
C               HNI(N,LIMP), HNIR(N,LIMP)
      dimension HNI(*),      HNIR(*)
C
      call HI ('FUPPI')
C     !BEG
      if(NO.gt.0) then
        IE = 0
  100   continue
          IS = IE+1
          IE = min(IE+8,N)
          call HUPPI (NO,IS,IE,LIMP,HNI ,N,'HN ')
          call HUPPI (NO,IS,IE,LIMP,HNIR,N,'HNR')
          if(IE.lt.N) goto 100
      end if
C     !END
      call BYE ('FUPPI')
C
      return
      end
