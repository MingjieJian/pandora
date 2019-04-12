      subroutine DOVIZI
     $(IU,IL,XLAM,LDL,DDL,K,XI,KODE,CDW,PGD)
C
C     Rudolf Loeser, 1991 Jun 17
C---- Sets up data for profile graphs.
C     !DASH
      save
C     !DASH
      real*8 CDW, DDL, PGD, XI, XLAM
      integer I, IL, IPEX, IU, J, K, KODE, LDL, LUEO
      logical DUMP
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
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external LINER, MESHED, MASHED, HI, BYE
C
C               DDL(LDL), XI(K), PGD(3,LDL)
      dimension DDL(*),   XI(*), PGD(3,*)
C
      call HI ('DOVIZI')
C     !BEG
      DUMP = (IPEX.lt.0).or.(IPEX.eq.4)
      if(DUMP) then
        call MESHED  ('DOVIZI', 2)
      end if
C
      do 101 I = 1,LDL
C
        PGD(1,I) = DDL(I)+XLAM
        if(KODE.eq.1) then
          PGD(2,I) = DDL(I)-CDW*XI(K)
        else
          PGD(2,I) = DDL(I)+CDW*XI(1)
        end if
        PGD(3,I) = DDL(I)+CDW*XI(K)
C
        if(DUMP) then
          call LINER (1, LUEO)
          write (LUEO,100) IU,IL,KODE,K,LDL,I,(PGD(J,I),J=1,3)
  100     format(' ','DOVIZI',6I5,1P3E20.12)
        end if
  101 continue
C
      if(DUMP) then
        call MASHED  ('DOVIZI')
      end if
C     !END
      call BYE ('DOVIZI')
C
      return
      end
