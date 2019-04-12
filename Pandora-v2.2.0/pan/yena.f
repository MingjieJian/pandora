      subroutine YENA
     $(LUMR,Z,TE,VM,VAMB,VBMB,VCMB,VDMB,VH,VP,VE,V1,V2,V3,GVL)
C
C     Rudolf Loeser, 1998 Jun 26
C---- Puts diffusion velocity data into restart file.
C     !DASH
      save
C     !DASH
      real*8 GVL, TE, V1, V2, V3, VAMB, VBMB, VCMB, VDMB, VE, VH, VM,
     $       VP, Z
      integer KAMB, KVLG, LUMR, MDFG, MDFV, MODE, N, NL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(106),MDFV )
      equivalence (KZQ(172),MDFG )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(32),KAMB )
      equivalence (LEST(47),KVLG )
C     !DASH
      external BUNT, PANT, YARDEN, HI, BYE
C
C               VAMB(N), VBMB(N), VCMB(N), VDMB(N), VM(N), TE(N), Z(N),
      dimension VAMB(*), VBMB(*), VCMB(*), VDMB(*), VM(*), TE(*), Z(*),
C
C               VH(N), VP(N), VE(N), V1(N), V2(N), V3(N), GVL(N,NL)
     $          VH(*), VP(*), VE(*), V1(*), V2(*), V3(*), GVL(*)
C
      data MODE /1/
C     !EJECT
C
      call HI ('YENA')
C     !BEG
      call YARDEN (LUMR,1,'DIFFUSION')
C
      write (LUMR,100) N,KAMB,KVLG
  100 format('N (',I4,' ) >   KAMB',I3,'  KVLG',I3)
C
      if(MDFV.gt.0) then
        call BUNT (LUMR,Z   ,'Z')
        call BUNT (LUMR,TE  ,'TE')
        call BUNT (LUMR,VM  ,'VMASS')
        call BUNT (LUMR,VAMB,'VAMB')
        call BUNT (LUMR,VBMB,'VBMB')
        call BUNT (LUMR,VCMB,'VCMB')
        call BUNT (LUMR,VDMB,'VDMB')
        call BUNT (LUMR,VH  ,'VH')
        call BUNT (LUMR,VP  ,'VP')
        call BUNT (LUMR,VE  ,'VE')
        call BUNT (LUMR,V1  ,'V1')
        call BUNT (LUMR,V2  ,'V2')
        call BUNT (LUMR,V3  ,'V3')
      end if
C
      if(MDFG.gt.0) then
        call PANT (LUMR,GVL,N,NL,MODE,'GVL')
      end if
C
      call YARDEN (LUMR,2,'DIFFUSION')
C     !END
      call BYE ('YENA')
C
      return
      end
