      subroutine NOUR
     $(XRD,ZRD,YRD,FJR,RXI,N,K)
C
C     Rudolf Loeser, 1988 Apr 01
C---- Saves debug checksums, for DELOS.
C     !DASH
      save
C     !DASH
      real*8 FJR, RXI, XRD, YRD, ZRD
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
C               FJR(N,K), RXI(N,K), XRD(N,K), YRD(N,K), ZRD(N,K)
      dimension FJR(*),   RXI(*),   XRD(*),   YRD(*),   ZRD(*)
C
      data TIT /'PRD2 ---'/
C
      call HI ('NOUR')
C     !BEG
      if(MO.gt.0) then
        NK = N*K
        call MISO    (TIT(9:33))
C
        TIT(6:8) = 'XRD'
        call CHECKER (XRD, 1, NK, TIT)
C
        TIT(6:8) = 'ZRD'
        call CHECKER (ZRD, 1, NK, TIT)
C
        TIT(6:8) = 'YRD'
        call CHECKER (YRD, 1, NK, TIT)
C
        TIT(6:8) = 'FJR'
        call CHECKER (FJR, 1, NK, TIT)
C
        TIT(6:8) = 'RXI'
        call CHECKER (RXI, 1, NK, TIT)
      end if
C     !END
      call BYE ('NOUR')
C
      return
      end
