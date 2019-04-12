      subroutine AQUAVIT
     $(LUAP,DMPA,N,Z,TE,ZT,XNE,HND,HK,H1,HEK,HE1,HE2K,HE21,ZI,Z1,Z2,Z3,
     $ ZXG,VAMB,VBMB,VCMB,VDMB,VE,VP,VH,V1,V2,V3,XION,ZION,DEE)
C
C     Rudolf Loeser, 1989 Sep 15.
C---- Prints, for FAITH.
C     (This is version 4 of AQUAVIT.)
C     !DASH
      save
C     !DASH
      real*8 DEE, FZION, H1, HE1, HE21, HE2K, HEK, HK, HND, TE, V1, V2,
     $       V3, VAMB, VBMB, VCMB, VDMB, VE, VH, VP, XION, XNE, Z, Z1,
     $       Z2, Z3, ZI, ZION, ZT, ZXG, ZXMIN
      integer I, LUAP, N
      logical DMPA
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
      equivalence (RZQ(112),ZXMIN)
      equivalence (RZQ(128),FZION)
C     !DASH
      external  LINER, MERULA, HI, BYE
C
C               XNE(N), HND(N), Z(N), TE(N), ZT(N), ZXG(N), DEE(4,5,N),
      dimension XNE(*), HND(*), Z(*), TE(*), ZT(*), ZXG(*), DEE(4,5,*),
C
C               ZI(N), VAMB(N), VP(N), VH(N), ZION(N), HEK(N), XION(N),
     $          ZI(*), VAMB(*), VP(*), VH(*), ZION(*), HEK(*), XION(*),
C
C               HK(N), HE1(N), HE2K(N), HE21(N), Z1(N), Z2(N), VDMB(N),
     $          HK(*), HE1(*), HE2K(*), HE21(*), Z1(*), Z2(*), VDMB(*),
C
C               Z3(N), VBMB(N), VCMB(N), VE(N), V1(N), V2(N), V3(N),
     $          Z3(*), VBMB(*), VCMB(*), VE(*), V1(*), V2(*), V3(*),
C
C               H1(N)
     $          H1(*)
C     !EJECT
C
      call HI ('AQUAVIT')
C     !BEG
      if(LUAP.gt.0) then
        if(DMPA) then
          call MERULA (DEE)
        end if
        call LINER    (2, LUAP)
        write (LUAP,101)
  101   format(' ',4X,7X,'Depth',11X,'H',11X,'H',11X,'H',4X,'Electron',
     $             8X,'He-I',8X,'He-I',7X,'He-II',7X,'He-II'/
     $         ' ',4X,8X,'(km)',7X,'total',5X,'level 1',9X,'ion',
     $             5X,'density',5X,'level 1',9X,'ion',
     $             5X,'level 1',9X,'ion'//
     $         ' ',4X,11X,'Z',10X,'NH',9X,'NH1',9X,'NHK',10X,'NE',8X,
     $            'NHE1',8X,'NHEK',7X,'NHE21',7X,'NHE2K')
        write (LUAP,102) (I,Z(I),HND(I),H1(I),HK(I),XNE(I),HE1(I),
     $                  HEK(I),HE21(I),HE2K(I),I=1,N)
  102   format(5(' ',I4,1P9E12.4/))
        call LINER    (2, LUAP)
        write (LUAP,103)
  103   format(' ',4X,60X,'Logarithmic gradients of:'/
     $         ' ',4X,24X,7X,'Depth',1X,7('----------'),'-',/
     $         ' ',4X,1X,'Temperature',5X,'NHK/NH1',2X,'multiplier',
     $             8X,'XION',8X,'He/H',4X,'HeII/HeI',2X,'HeIII/HeII',
     $             4X,'HeIII/He',10X,'TE'//
     $         ' ',4X,10X,'TE',8X,'XION',8X,'ZION',10X,'ZI',10X,'Z1',
     $             10X,'Z2',10X,'Z3',9X,'ZXG',10X,'ZT')
        write (LUAP,104) (I,TE(I),XION(I),ZION(I),ZI(I),Z1(I),Z2(I),
     $                  Z3(I),ZXG(I),ZT(I),I=1,N)
  104   format(5(' ',I4,1P9E12.4/))
        write (LUAP,105) ZXMIN,FZION
  105   format(' ',23X,'ZXMIN',1PE12.4/
     $         ' ',23X,'FZION',  E12.4)
        call LINER    (2, LUAP)
        write (LUAP,106)
  106   format(' ',4X,10X,'Ambipolar Velocities (cm/s):'/
     $         ' ',4X,1X,4('----------'),'-------'/
     $         ' ',4X,9X,'H-I',4X,'Hydrogen',8X,'He-I',
     $             7X,'He-II',21X,'Diffusion Velocities (cm/s) for:'/
     $         ' ',4X,4X,'relative',4X,'relative',4X,'relative',
     $             4X,'relative',2X,7('----------')/
     $         ' ',4X,5X,'to H-II',3X,'to Helium',2X,'to He-ions',
     $             3X,'to He-III',3X,'Electrons',5X,'Protons',
     $             4X,'Hydrogen',8X,'He-I',7X,'He-II',6X,'He-III'//
     $         ' ',4X,8X,'VAMB',8X,'VBMB',8X,'VCMB',8X,'VDMB',10X,
     $             'VE',10X,'VP',10X,'VH',10X,'V1',10X,'V2',10X,'V3')
        write (LUAP,107) (I,VAMB(I),VBMB(I),VCMB(I),VDMB(I),VE(I),
     $                  VP(I),VH(I),V1(I),V2(I),V3(I),I=1,N)
  107   format(5(' ',I4,1P10E12.4/))
      end if
C     !END
      call BYE ('AQUAVIT')
C
      return
      end
