      subroutine KURASH
     $(NAB,BANDL,BANDU,WAVE,MEMBER,IB)
C
C     Rudolf Loeser, 1983 Jun 29
C---- Sets MEMBER=.true. if wave falls into one of the intervals
C     BANDL(I) .le. WAVE .le. BANDU(I), 1 .le. I .le. NAB;
C     and =.false. otherwise.
C---- Also returns IB, the band index, if appropriate.
C     !DASH
      save
C     !DASH
      real*8 BANDL, BANDU, WAVE
      integer I, IB, NAB
      logical MEMBER
C     !DASH
      external HI, BYE
C
C               BANDL(NAB), BANDU(NAB)
      dimension BANDL(*),   BANDU(*)
C
      call HI ('KURASH')
C     !BEG
      MEMBER = .false.
C
      IB = 0
      if(NAB.gt.0) then
        do 100 I = 1,NAB
          IB = I
          MEMBER = (WAVE.ge.BANDL(I)).and.(WAVE.le.BANDU(I))
          if(MEMBER) goto 101
  100   continue
        IB = 0
      end if
  101 continue
C     !END
      call BYE ('KURASH')
C
      return
      end
