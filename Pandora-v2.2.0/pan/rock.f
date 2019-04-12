      subroutine ROCK
     $(N,CQI,MQT,CQTAIL)
C
C     Rudolf Loeser, 1976 Dec 02
C---- Smoothes out a raw CQI-table, for GRAVEL.
C     !DASH
      save
C     !DASH
      real*8 CQI, CQTAIL, ONE
      integer I, IDN, IUP, J, MQT, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  HI, BYE
      intrinsic max
C
C               CQI(N), CQTAIL(MQT)
      dimension CQI(*), CQTAIL(*)
C
      call HI ('ROCK')
C     !BEG
      do 101 I = 1,N
        if(CQI(I).eq.ONE) then
          IDN = I
          IUP = I
C
          do 100 J = 1,MQT
            IDN = IDN-1
            if(IDN.gt.0) then
              CQI(IDN) = max(CQI(IDN),CQTAIL(J))
            end if
C
            IUP = IUP+1
            if(IUP.le.N) then
              CQI(IUP) = max(CQI(IUP),CQTAIL(J))
            end if
  100     continue
C
        end if
  101 continue
C     !END
      call BYE ('ROCK')
C
      return
      end
