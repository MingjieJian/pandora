      subroutine JANE
     $(N,Z,DW,DP,VX,XNE,FDDL,LDL,NR,ZR,DWR,DPR,VXR,XNER,FDLR)
C
C     Rudolf Loeser, 1978 Jun 14
C---- Sets up reduced tables, for PEEK.
C     (This is version 2 of JANE.)
C     !DASH
      save
C     !DASH
      real*8 DP, DPR, DW, DWR, FDDL, FDLR, VX, VXR, XNE, XNER, Z, ZR
      integer I, J, L, LDL, N, NANA1, NANA2, NR
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
      equivalence (KZQ(135),NANA1)
      equivalence (KZQ(137),NANA2)
C     !DASH
      external  HI, BYE
      intrinsic min
C
C               DW(N), DPR(N ,LDLMX), DP(N,LDLMX), VX(N), XNE(N), Z(N),
      dimension DW(*), DPR(NR,*),     DP(N,*),     VX(*), XNE(*), Z(*),
C
C               ZR(NR), DWR(NR), VXR(NR), XNER(NR), FDLR(NR), FDDL(N)
     $          ZR(*),  DWR(*),  VXR(*),  XNER(*),  FDLR(*),  FDDL(*)
C
      call HI ('JANE')
C     !BEG
      J = NANA1-NANA2
C
      do 101 I = 1,NR
C
        J = min(J+NANA2,N)
C
        ZR (I)  = Z(J)
        DWR(I)  = DW(J)
        VXR(I)  = VX(J)
        XNER(I) = XNE(J)
        FDLR(I) = FDDL(J)
C
        do 100 L = 1,LDL
          DPR(I,L) = DP(J,L)
  100   continue
C
  101 continue
C     !END
      call BYE ('JANE')
C
      return
      end
