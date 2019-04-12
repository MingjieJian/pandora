      subroutine CONSUL
     $(LU,LUAP,MN1,N,NL,Z,VE,VP,VH,V1,V2,V3,VM,ZT,ZI,Z1,Z2,Z3,GVI,GVL,
     $ TE,HND,H1,HK,XNE,HE1,HE2,HE3,RGVL,DIDRGVL,KAMB,KVLG,W)
C
C     Rudolf Loeser, 1998 Mar 16
C---- Supervises diffusion plots.
C     (This is version 2 of CONSUL.)
C     !DASH
      save
C     !DASH
      real*8 GVI, GVL, H1, HE1, HE2, HE3, HK, HND, RGVL, TE, V1, V2, V3,
     $       VE, VH, VM, VP, W, XNE, Z, Z1, Z2, Z3, ZI, ZT
      integer JQ, JX, KAMB, KION, KODE, KVLG, LP, LU, LUAP, MN1, N, NL
      logical DIDRGVL, DOTHEM
      character LAB*7
C     !DASH
      external  MENAI, FANDAR, KAILUR, LAKANE, KANALE, GARDEN, HI, BYE
      intrinsic max
C
      dimension W(*)
C
C               GVL(N,NL), GVI(N), HND(N), XNE(N), VP(N), VH(N), V1(N),
      dimension GVL(N,*),  GVI(*), HND(*), XNE(*), VP(*), VH(*), V1(*),
C
C               V2(N), V3(N), VM(N), ZT(N), ZI(N), Z1(N), Z2(N), Z3(N),
     $          V2(*), V3(*), VM(*), ZT(*), ZI(*), Z1(*), Z2(*), Z3(*),
C
C               RGVL(N,NL), H1(N), HK(N), VE(N), HE1(N), HE2(N), TE(N),
     $          RGVL(N,*),  H1(*), HK(*), VE(*), HE1(*), HE2(*), TE(*),
C
C               Z(N), HE3N(N)
     $          Z(*), HE3(*)
C
      data KODE, LAB /2, 'Z-index'/
C          KODE = 1 for (original) long plot;
C          KODE = 2 for one-page plot.
C     !EJECT
C
      call HI ('CONSUL')
C     !BEG
      KION = max(KAMB,KVLG)
      if((LU.gt.0).and.(KION.gt.0)) then
C
        LP = max(LU,LUAP)
        call MENAI (TE, N, LP, DOTHEM, JQ, JX)
C
        if(DOTHEM) then
C
          if(KAMB.gt.0) then
C----       1) Velocities
            call FANDAR (LUAP, KODE, LAB, N, Z, JQ, JX, VE, VP, VH,
     $                   V1, V2, V3)
          end if
C
C----     2) Derivatives
          call KAILUR   (LUAP, KODE, LAB, N, Z, JQ, JX, ZT, ZI,
     $                   Z1, Z2, Z3)
C
C----     3) "Total Terms"
          call LAKANE   (LU, KODE, LAB, N, 1, NL, MN1, Z, JQ, JX, GVL)
          call LAKANE   (LU, KODE, LAB, N, 3, NL, MN1, Z, JQ, JX, GVL)
          call LAKANE   (LU, KODE, LAB, N, 5, NL, MN1, Z, JQ, JX, GVL)
          call LAKANE   (LU, KODE, LAB, N, 8, NL, MN1, Z, JQ, JX, GVL)
C
          if(DIDRGVL) then
C----       4) Diffusion term ratios
            call KANALE (LU, KODE, LAB, N, NL, MN1, Z, JQ, JX, RGVL)
          end if
        end if
C
C----   Number Densities
        call GARDEN     (LU, N, TE, HND, H1, HK, XNE, HE1, HE2, HE3, W)
C
      end if
C     !END
      call BYE ('CONSUL')
C
      return
      end
