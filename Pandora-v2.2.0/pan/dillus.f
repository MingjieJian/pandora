      subroutine DILLUS
     $(XT,YT,NT,XF,YF,NF,LI)
C
C     Rudolf Loeser, 1984 Mar 20
C---- Processes extrapolations at start of table, for AGLAURA.
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
      call HI ('DILLUS')
C     !BEG
      LI = 0
C
      if(XT(1).lt.XF(1)) then
C----   Find limiting index of extrapolation
        do 100 I = 1,NT
          if(XT(I).ge.XF(1)) then
            goto 101
          end if
          LI = I
  100   continue
C
  101   continue
C
        if(LI.gt.0) then
C----     Process extrapolated portion
          call DULLES (YT, LI)
        end if
      end if
C     !END
      call BYE ('DILLUS')
C
      return
      end
