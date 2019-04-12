      subroutine JOUR
     $(VXI,XQSF,XRD,ZRD,YRD,N,K)
C
C     Rudolf Loeser, 1988 Apr 01
C---- Saves debug checksums, for CATANA.
C     !DASH
      save
C     !DASH
      real*8 VXI, XQSF, XRD, YRD, ZRD
      integer K, MO, N, NK
      character TIT*40
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external CHECKER, MISO, HI, BYE
C
C               XRD(N,K), YRD(N,K), VXI(N,K), XQSF(N,K), ZRD(N,K)
      dimension XRD(*),   YRD(*),   VXI(*),   XQSF(*),   ZRD(*)
C
      data TIT /'PRD1 ---'/
C
      call HI ('JOUR')
C     !BEG
      if(MO.gt.0) then
        NK = N*K
        call MISO    (TIT(9:33))
C
        TIT(6:8) = 'VXI'
        call CHECKER (VXI,  1, NK, TIT)
C
        TIT(6:8) = 'QSF'
        call CHECKER (XQSF, 1, NK, TIT)
C
        TIT(6:8) = 'XRD'
        call CHECKER (XRD,  1, NK, TIT)
C
        TIT(6:8) = 'ZRD'
        call CHECKER (ZRD,  1, NK, TIT)
C
        TIT(6:8) = 'YRD'
        call CHECKER (YRD,  1, NK, TIT)
      end if
C     !END
      call BYE ('JOUR')
C
      return
      end
