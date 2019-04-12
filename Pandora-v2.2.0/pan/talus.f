      subroutine TALUS
     $(TE,V,VR,EMU,N,WVL,IVSW,DW)
C
C     Rudolf Loeser, 1983 Dec 05
C---- Computes a table of Doppler Width for the transition whose core
C     wavelength is WVL.
C
C---- If IVSW .eq. 0, then
C     the tables VR and EMU are not used, and DW is independent of
C     direction.
C
C---- If IVSW .gt. 0, then
C     EMU is the table of cosines of look angle along the current ray,
C     V is the table of Tangential Broadening Velocity, VR is the table
C     of Radial Broadening Velocity, and DW depends on the direction of
C     the current ray,
C
C     (This is version 2 of TALUS).
C     !DASH
      save
C     !DASH
      real*8 AMASS, DNU, DW, EMU, TE, V, V2, VR, WVL, dummy
      integer I, IVSW, N
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
      equivalence (RZQ(  4),AMASS)
C     !DASH
      external SARI, DOPPLER, ANGIE, HI, BYE
C
C               TE(N), V(N), VR(N), EMU(N), DW(N)
      dimension TE(*), V(*), VR(*), EMU(*), DW(*)
C
      call HI ('TALUS')
C     !BEG
      call ANGIE     (WVL, DNU)
      do 100 I = 1,N
        call SARI    (V(I), VR(I), EMU(I), IVSW, V2)
        call DOPPLER (DNU, TE(I), AMASS, V2, dummy, DW(I))
  100 continue
C     !END
      call BYE ('TALUS')
C
      return
      end
