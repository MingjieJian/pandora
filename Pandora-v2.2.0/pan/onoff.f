      subroutine ONOFF
     $(I,ILIM,TEXT)
C
C     Rudolf Loeser, 1989 Oct 31
C---- Computes ILIM from I, such that:
C     ILIM = 0 if I .le. 0, and
C     ILIM = 1 if I .ge. 1; and
C     sets TEXT = 'off' if ILIM=0, = 'on ' if ILIM=1.
C     !DASH
      save
C     !DASH
      integer I, ILIM
      character TEXT*3
C     !DASH
      external  HI, BYE
      intrinsic min, max
C
      call HI ('ONOFF')
C     !BEG
      ILIM = min(max(I,0),1)
      if(ILIM.ge.1) then
        TEXT = 'on '
      else
        TEXT = 'off'
      end if
C     !END
      call BYE ('ONOFF')
C
      return
      end
