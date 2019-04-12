      subroutine TAIFUN
     $(N,IWS,ISIG)
C
C     Rudolf Loeser, 1978 Jul 26
C---- Sets up markers, for SNOW.
C     (This is version 2 of TAIFUN.)
C     !DASH
      save
C     !DASH
      real*8 CD, CU, SUM
      integer ID, IMAX, IMIN, ISIG, IU, IWS, JD, JU, L1, L2, LSIG, MD,
     $        MSIG, MU, N
C     !DASH
      external  SETI, MINMAXI, SLOUGH, CORD, FINGER, HI, BYE
C
C               IWS(N), ISIG(N)
      dimension IWS(*), ISIG(*)
C
      data MSIG,LSIG /1, 0/
C     !EJECT
C
      call HI ('TAIFUN')
C     !BEG
C---- Initialize marker string
      call SETI (ISIG,1,N,LSIG)
C---- Find MAX and mark it
      call MINMAXI      (IWS,1,N,IMIN,IMAX)
C
      if(IWS(IMAX).gt.0) then
        ISIG(IMAX) = MSIG
C----   Initialize scan sum
        SUM = IWS(IMAX)
C----   Initialize bidirectional scan controls - U=up, D=down
C       starting indices for scans
        IU = IMAX
        ID = IMAX
C       Contribution status flags - 1=exists, 0=does not exist
        MU = 1
        MD = 1
C       Scan status flag - 1=do it, 0=do not do it
        JU = 1
        JD = 1
C       Marker switch - 1=has been set, 0=has not been set
        L1 = 0
        L2 = 0
C
C----   Scan for next contributions
  100   continue
          call SLOUGH   (IWS,N,IU,MU,JU,CU,ID,MD,JD,CD)
C----     Choose contribution
          call CORD     (SUM,CU,MU,JU,CD,MD,JD)
          if((MU+MD).gt.0) then
C----       Set markers if possible
            call FINGER (SUM,ISIG,L1,L2,IU,ID)
C----       Loop back
            if((L1+L2).lt.2) then
              goto 100
            end if
          end if
C
      end if
C     !END
      call BYE ('TAIFUN')
C
      return
      end
