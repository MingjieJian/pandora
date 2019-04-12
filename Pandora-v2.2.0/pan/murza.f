      subroutine MURZA
     $(REMARK,M)
C
C     Rudolf Loeser, 1990 Dec 04
C---- Sets up part of a plot label.
C     !DASH
      save
C     !DASH
      integer M
      character REMARK*8
C     !DASH
      external HI, BYE
C
      call HI ('MURZA')
C     !BEG
      if(M.gt.26) then
        REMARK = ' partial'
      else
        REMARK = 'complete'
      end if
C     !END
      call BYE ('MURZA')
C
      return
      end
