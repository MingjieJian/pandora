      subroutine STICK
     $(KODE,XN1,SN1V,XNK,SNKV)
C
C     Rudolf Loeser, 2000 Jul 18
C---- Saves a set of Special-N1 and/or Special-NK values,
C     for iterative analysis.
C     (This is version 2 of STICK.)
C     !DASH
      save
C     !DASH
      real*8 SN1V, SNKV, XN1, XNK
      integer KODE, MN1
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
C
C---- SPONNE      as of 2000 Dec 18
      integer     N1SKNT,NKSKNT
      common      /SPONNE/ N1SKNT,NKSKNT
C     Control parameter for Special-N1,NK iterative summary
C     .
C     !DASH
      external MOVE1, HI, BYE
C
C               ITMX = 3*ITN1R+1
C
C               XN1(N), SN1V(N  ,ITMX), XNK(N), SNKV(N  ,ITMX)
      dimension XN1(*), SN1V(MN1,*),    XNK(*), SNKV(MN1,*)
C
      call HI ('STICK')
C     !BEG
      if((KODE.eq.1).or.(KODE.eq.3)) then
        N1SKNT = N1SKNT+1
        call MOVE1 (XN1,MN1,SN1V(1,N1SKNT))
      end if
      if((KODE.eq.2).or.(KODE.eq.3)) then
        NKSKNT = NKSKNT+1
        call MOVE1 (XNK,MN1,SNKV(1,NKSKNT))
      end if
C     !END
      call BYE ('STICK')
C
      return
      end
