      subroutine CAUTION
     $(T5,N,SIGNAL)
C
C     Rudolf Loeser, 1980 Jul 25
C---- Returns with SIGNAL=.true. if finished with TAU5000 Adjustment
C     iterations, =.false. if not.
C     (This is version 2 of CAUTION.)
C     !DASH
      save
C     !DASH
      real*8 T5, THI, TLO
      integer N
      logical SIGNAL
C     !DASH
      external HI, BYE
C
C               T5(N)
      dimension T5(*)
C
      data TLO,THI /9.9D-1, 1.01D0/
C
      call HI ('CAUTION')
C     !BEG
      if(N.le.1) then
        SIGNAL = .false.
C
      else if(N.ge.11) then
        SIGNAL = .true.
C
      else if((T5(N).gt.TLO).and.(T5(N).lt.THI)) then
        SIGNAL = .true.
C
      else
        SIGNAL = .false.
      end if
C     !END
      call BYE ('CAUTION')
C
      return
      end
