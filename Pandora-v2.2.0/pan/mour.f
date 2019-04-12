      subroutine MOUR
     $(VXI,XQSF,SNU,SLR,N,K)
C
C     Rudolf Loeser, 2005 Apr 04
C---- Saves debug checksums, for CIMON.
C     !DASH
      save
C     !DASH
      real*8 SLR, SNU, VXI, XQSF
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
C               XQSF(N,K), SLR(N,K), VXI(N,K), SNU(N,K)
      dimension XQSF(*),   SLR(*),   VXI(*),   SNU(*)
C
      data TIT /'PRD4 ---'/
C
      call HI ('MOUR')
C     !BEG
      if(MO.gt.0) then
        NK = N*K
        call MISO    (TIT(9:33))
C
        TIT(6:8) = 'VXI'
        call CHECKER (VXI,  1, NK, TIT)
C
        TIT(6:8) = 'XQSF'
        call CHECKER (XQSF, 1, NK, TIT)
C
        TIT(6:8) = 'SNU'
        call CHECKER (SNU,  1, NK, TIT)
C
        TIT(6:8) = 'SLR'
        call CHECKER (SLR,  1, NK, TIT)
      end if
C     !END
      call BYE ('MOUR')
C
      return
      end
