      subroutine DINSON
     $(XT,YT,NT,XF,YF,NF,LI)
C
C     Rudolf Loeser, 1984 Mar 20
C---- Processes extrapolations at end of table, for AGLAURA.
C     !DASH
      save
C     !DASH
      real*8 XF, XT, YF, YT
      integer I, LI, NF, NT
C     !DASH
      external DULLES, HI, BYE
C
C               XT(NT), YT(NT), XF(NF), YF(NF)
      dimension XT(*),  YT(*),  XF(*),  YF(*)
C
      call HI ('DINSON')
C     !BEG
      LI = 0
C
      if(XT(NT).gt.XF(NF)) then
C----   Find limiting index of extrapolation
        do 100 I = NT,1,-1
          if(XT(I).le.XF(NF)) then
            goto 101
          end if
          LI = I
  100   continue
C
  101   continue
C
        if(LI.gt.0) then
C----     Process extrapolated portion
          call DULLES (YT(LI), (NT-LI+1))
        end if
      end if
C     !END
      call BYE ('DINSON')
C
      return
      end
