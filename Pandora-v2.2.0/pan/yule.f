      subroutine YULE
     $(LL,KODE,N,DP,DW,VX,PHI)
C
C     Rudolf Loeser, 1982 Sep 22
C---- Dumps, for EPIC.
C     (This is version 3 of YULE.)
C     !DASH
      save
C     !DASH
      real*8 DP, DW, PHI, VX
      integer IPR01, IPR02, KODE, LL, LUEO, N
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
      equivalence (KZQ( 57),IPR01)
      equivalence (KZQ( 58),IPR02)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external VECOUT, HI, BYE
C
C               DP(N), DW(N), VX(N), PHI(N)
      dimension DP(*), DW(*), VX(*), PHI(*)
C
      call HI ('YULE')
C     !BEG
      if((LL.ge.IPR01).and.(LL.le.IPR02)) then
        if(KODE.eq.1) then
          KODE = 0
          call VECOUT (LUEO, DP , N, 'DP')
          call VECOUT (LUEO, DW , N, 'DW')
          call VECOUT (LUEO, VX , N, 'VXS')
        end if
        call VECOUT   (LUEO, PHI, N, 'PHI')
      end if
C     !END
      call BYE ('YULE')
C
      return
      end
