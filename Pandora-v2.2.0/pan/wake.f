      subroutine WAKE
     $(N,WR1,RKW,WEIT,XINCH,WMN,WMX,SMP,KMSS,ORK1,RKO,RKI,LABEL)
C
C     Rudolf Loeser, 1974 Dec 27
C---- Weights RK1.
C     !DASH
      save
C     !DASH
      real*8 ONE, ORK1, RKI, RKO, RKW, SMP, WEIT, WMN, WMX, WR1, XINCH
      integer KLOG, KMSS, MODE, N
      character LABEL*(*)
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ORCHID, WEITER, HI, BYE
C
C               WEIT(N), RKW(N), ORK1(N), RKO(N), RKI(N), WR1(N)
      dimension WEIT(*), RKW(*), ORK1(*), RKO(*), RKI(*), WR1(*)
C
      data KLOG,MODE /1, 1/
C
      call HI ('WAKE')
C     !BEG
      call ORCHID (N,WR1,RKW,XINCH,WMX,WMN,SMP,ORK1,RKO,RKI)
      call WEITER (RKI,RKI,RKO,WR1,ONE,N,KLOG,MODE,KMSS,LABEL,WEIT)
C     !END
      call BYE ('WAKE')
C
      return
      end
