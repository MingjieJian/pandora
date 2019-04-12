      subroutine THEODOR
     $(MRP,JLEV,XNU,XNUC,RNU,RCP,SIG)
C
C     Rudolf Loeser, 1982 Dec 13
C---- Does interpolations, for BEEFUP.
C     (This is version 5 of THEODOR.)
C     !DASH
      save
C     !DASH
      real*8 RCP, RNU, SIG, WII, WLL, WLU, WR, XNU, XNUC
      integer II, JLEV, LL, LU, MRP
C     !DASH
      external DIVIDE, NOMAD, HI, BYE
C
C               RNU(MRX+1), RCP(MRX+1), XNU(NSL), XNUC(NSL)
      dimension RNU(*),     RCP(*),     XNU(*),   XNUC(*)
C
      call HI ('THEODOR')
C     !BEG
      do 101 II = 2,(MRP-1)
        if(RCP(II).eq.SIG) then
          LL = II-1
C
          LU = II
  100     continue
            LU = LU+1
            if(RCP(LU).eq.SIG) goto 100
C
          call NOMAD  (RNU(LL), XNU, XNUC, JLEV, WLL)
          call NOMAD  (RNU(II), XNU, XNUC, JLEV, WII)
          call NOMAD  (RNU(LU), XNU, XNUC, JLEV, WLU)
C
          call DIVIDE ((WII-WLL), (WLU-WLL), WR)
          RCP(II) = RCP(LL)+(RCP(LU)-RCP(LL))*WR
        end if
  101 continue
C     !END
      call BYE ('THEODOR')
C
      return
      end
