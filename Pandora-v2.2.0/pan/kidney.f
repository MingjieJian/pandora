      subroutine KIDNEY
     $(NCP,WAVESB,BANDL,BANDU,NB,IS,IE)
C
C     Rudolf Loeser, 1983 Oct 28
C---- Finds limiting indices of current band, for HEART.
C     !DASH
      save
C     !DASH
      real*8 BANDL, BANDU, WAVESB
      integer I, IE, IS, NB, NCP
C     !DASH
      external HI, BYE
C
C               WAVESB(NCP)
      dimension WAVESB(*)
C
      call HI ('KIDNEY')
C     !BEG
      IS = 0
      do 100 I = 1,NCP
        if((BANDL.le.WAVESB(I)).and.(BANDU.ge.WAVESB(I))) then
          IE = I
          if(IS.eq.0) then
            IS = I
          end if
        else
          if(IS.ne.0) then
            goto 101
          end if
        end if
  100 continue
C
  101 continue
      NB = IE-(IS-1)
C     !END
      call BYE ('KIDNEY')
C
      return
      end
