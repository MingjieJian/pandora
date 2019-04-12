      subroutine URSUS
     $(MN1,KAMB,KVLG,N,NL,XND,GHI,GHL,GXI,GXL,GVI,GVL)
C
C     Rudolf Loeser, 1989 Nov 13
C---- Produces checksums for TARPON.
C     (This is version 3 of URSUS.)
C     !DASH
      save
C     !DASH
      real*8 GHI, GHL, GVI, GVL, GXI, GXL, XND
      integer IOVER, KAMB, KVLG, MN1, N, NL, NNL
      character TIT*12
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST( 2),IOVER)
C     !DASH
      external CHECKER, HI, BYE
C
C               XND(N,NL), GHI(N), GHL(N,NL), GXI(N), GXL(N,NL), GVI(N),
      dimension XND(*),    GHI(*), GHL(*),    GXI(*), GXL(*),    GVI(*),
C
C               GVL(N,NL)
     $          GVL(*)
C
      call HI ('URSUS')
C     !BEG
      write (TIT,100) IOVER
  100 format(', IOVER =',I3)
      NNL = N*NL
C
      if(MN1.ge.2) then
        call CHECKER (XND,1,N  ,'Diffusion - N1'//TIT)
      end if
C
      if(KAMB.gt.0) then
        call CHECKER (GHI,1,N  ,'Diffusion - GH-I'//TIT)
        call CHECKER (GHL,1,NNL,'Diffusion - GH-L'//TIT)
      end if
C
      if(KVLG.gt.0) then
        call CHECKER (GXI,1,N  ,'Diffusion - GX-I'//TIT)
        call CHECKER (GXL,1,NNL,'Diffusion - GX-L'//TIT)
      end if
C
      call CHECKER   (GVI,1,N  ,'Diffusion - GVN-I'//TIT)
      call CHECKER   (GVL,1,NNL,'Diffusion - GNV-L'//TIT)
C     !END
      call BYE ('URSUS')
C
      return
      end
