      subroutine WAXESS
     $(I,J,IU,IL, KODE,WARR,MUL,NT, WVAL,OK)
C
C     Rudolf Loeser, 1999 Aug 30
C---- Basic processing for WAITER.
C     !DASH
      save
C     !DASH
      real*8 WARR, WVAL
      integer I, IL, INIJ, INUL, IU, J, KODE, MUL, NT
      logical NOTNULL, OK
C     !DASH
      external  INDXUL, INDXNT, HI, BYE
C
C               WARR(MUL,NT)
      dimension WARR(MUL,*)
C
      call HI ('WAXESS')
C     !BEG
      call INDXUL   (I,J,INIJ)
      OK = (INIJ.ge.1).and.(INIJ.le.MUL)
      if(OK) then
        call INDXNT (IU,IL, NOTNULL,INUL)
        OK = (INUL.ge.1).and.(INUL.le.NT).and.NOTNULL
        if(OK) then
          if(KODE.eq.1) then
            WARR(INIJ,INUL) = WVAL
          else
            WVAL = WARR(INIJ,INUL)
          end if
        end if
      end if
C     !END
      call BYE ('WAXESS')
C
      return
      end
