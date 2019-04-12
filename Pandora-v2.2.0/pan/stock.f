      subroutine STOCK
     $(LU,SN1V,SNKV,PRAT)
C
C     Rudolf Loeser, 1998 Feb 04
C---- Iterative analysis for "Special N1" and "Special-NK",
C     from diffusion calculations.
C     Special version of CROW --- q.v.
C     !DASH
      save
C     !DASH
      real*8 PRAT, SN1V, SNKV, WSN1D
      integer INP, LU, MN1, MODE
      character TITLEK*15, TITLEN*15
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
      equivalence (KZQ(101),MN1  )
      equivalence (RZQ(155),WSN1D)
C
C---- SPONNE      as of 2000 Dec 18
      integer     N1SKNT,NKSKNT
      common      /SPONNE/ N1SKNT,NKSKNT
C     Control parameter for Special-N1,NK iterative summary
C     .
C     !DASH
      external SUGAR, SLENDER, SPICY, IDATH, LINER, HI, BYE
C
C               ITMX = ITN1R+1
C
C               SN1V(N,ITMX), SNKV(N,ITMX), PRAT(N)
      dimension SN1V(*),      SNKV(*),      PRAT(*)
C
      data TITLEN /'Special N-1    '/
      data TITLEK /'Special NK     '/
      data INP,MODE /1, 0/
C     !EJECT
C
      call HI ('STOCK')
C     !BEG
      if(LU.gt.0) then
        if(N1SKNT.gt.0) then
          call SUGAR   (MN1,N1SKNT,SN1V)
C
          call SLENDER (LU,TITLEN)
          call SPICY   (LU,MN1,N1SKNT,INP,SN1V)
          call LINER   (1,LU)
          write (LU,100) WSN1D
  100     format(' ','Note: WSN1D =',1PE11.4)
C
          call IDATH   (LU,MN1,SN1V,PRAT,N1SKNT,INP,TITLEN,MODE)
        end if
C
        if(NKSKNT.gt.0) then
          call SUGAR   (MN1,NKSKNT,SNKV)
C
          call SLENDER (LU,TITLEK)
          call SPICY   (LU,MN1,NKSKNT,INP,SNKV)
          call LINER   (1,LU)
          write (LU,100) WSN1D
C
          call IDATH   (LU,MN1,SNKV,PRAT,NKSKNT,INP,TITLEK,MODE)
        end if
      end if
C     !END
      call BYE ('STOCK')
C
      return
      end
