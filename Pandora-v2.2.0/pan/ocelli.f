      subroutine OCELLI
     $(N,DX,TE,V,VR,VXP,EMU,DW,DP,PHI,LABEL)
C
C     Rudolf Loeser, 2004 Feb 20
C---- Dumps details of DW-recomputation.
C
C     See also KELLI.
C     !DASH
      save
C     !DASH
      real*8 DP, DW, DX, EMU, PHI, TE, V, VR, VXP
      integer I, IDWIN, KOUNT, LUEO, N
      character LABEL*(*)
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 75),IDWIN)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external  LINER, MESHED, MASHED, HI, BYE
      intrinsic mod
C
      dimension DX(*), TE(*), VR(*), VXP(*), DP(*), DW(*), PHI(*), V(*),
     $          EMU(*)
C
      data KOUNT /0/
C     !EJECT
C
      call HI ('OCELLI')
C     !BEG
      KOUNT = KOUNT+1
      if(mod(KOUNT,IDWIN).eq.0) then
        call MESHED ('OCELLI', 2)
        write (LUEO,100) KOUNT,IDWIN,LABEL
  100   format(' ','Details of DW-recomputation',68X,2I13/
     $         ' ',A)
        call LINER  (1, LUEO)
        write (LUEO,101)
  101   format(' ',16X,'X',11X,'TE',12X,'V',11X,'VR',10X,'VXP',
     $             11X,'MU',11X,'DW',11X,'DP',10X,'PHI')
        call LINER  (1, LUEO)
        write (LUEO,102) (I,DX(I),TE(I),V(I),VR(I),VXP(I),EMU(I),DW(I),
     $                    DP(I),PHI(I),I=1,N)
  102   format(5(' ',I4,1P9E13.5/))
        call MASHED ('OCELLI')
      end if
C     !END
      call BYE ('OCELLI')
C
      return
      end
