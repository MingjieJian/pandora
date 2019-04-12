      subroutine RONBU
     $(N,MRR,R1N,Z,SIS,SES,SFS,DISK,FRR,DIS)
C
C     Rudolf Loeser, 1998 Jun 29
C---- Saves "averages" in restart file, for BRUNO.
C     (Note: the averages reside in column 1 of the associated arrays.)
C     !DASH
      save
C     !DASH
      real*8 DIS, FRR, R1N, SES, SFS, SIS, Z
      integer JSTCN, LUEO, LUMR, MODE, MRR, N
      logical DISK
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
      equivalence (KZQ( 35),JSTCN)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(29),LUMR )
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external YARDEN, DUCK, BUNT, PUNT, HI, BYE
C
C               SIS(N,Nmkuse), SES(N,Nmkuse), SFS(N,Nmkuse), FRR(MRR),
      dimension SIS(*),        SES(*),        SFS(*),        FRR(*),
C
C               DIS(MRR,Nmkuse), Z(N)
     $          DIS(*),          Z(*)
C
      data MODE /1/
C
      call HI ('RONBU')
C     !BEG
      if(JSTCN.gt.0) then
        call DUCK (LUMR, LUEO)
      end if
C
      call YARDEN (LUMR, 1, 'AVCON')
C
      write (LUMR,100) N,MRR,R1N
  100 format('N (',I4,')  MRR (',I4,')  R1N (',1PE16.8,') > ')
C
      call BUNT   (LUMR, Z,   'Z')
      call BUNT   (LUMR, SIS, 'I')
      call BUNT   (LUMR, SES, 'E')
      call BUNT   (LUMR, SFS, 'F')
      if(DISK) then
        call PUNT (LUMR, FRR, MRR, MODE, 'FRR')
        call PUNT (LUMR, DIS, MRR, MODE, 'I')
      end if
C
      call YARDEN (LUMR, 2, 'AVCON')
C     !END
      call BYE ('RONBU')
C
      return
      end
