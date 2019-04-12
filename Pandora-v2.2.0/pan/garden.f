      subroutine GARDEN
     $(LU,N,TE,HND,H1,HK,XNE,HE1,HE2,HE3,W)
C
C     Rudolf Loeser, 1997 Mar 25
C---- Plots H and He number densities, for FAITH.
C     !DASH
      save
C     !DASH
      real*8 H1, HE1, HE2, HE3, HK, HND, TE, W, XNE, dummy
      integer IFL, IMIN, IN, IS, IXX, LU, MOX, N, jummy
C     !DASH
      external POST, MINMAXD, FENCE, WGIVE, HI, BYE
C
      dimension W(*)
C
C               HND(N), H1(N), HK(N), XNE(N), HE1(N), HE2(N), HE3(N),
      dimension HND(*), H1(*), HK(*), XNE(*), HE1(*), HE2(*), HE3(*),
C
C               TE(N)
     $          TE(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IXX   ),(IN( 2),IFL   )
C
      call HI ('GARDEN')
C     !BEG
      if(LU.gt.0) then
C       (Get, and allocate, W allotment)
        call POST    (IN,IS,MOX,'GARDEN')
C
        call MINMAXD (TE,1,N,IMIN,jummy)
C
C----   Plot as function of log(TE)
        call FENCE   (LU,IMIN,TE,HND,H1,HK,XNE,HE1,HE2,HE3,W(IXX),
     $                W(IFL),1)
C
C----   Plot vs. depth index
        call FENCE   (LU,IMIN,dummy,HND,H1,HK,XNE,HE1,HE2,HE3,W(IXX),
     $                W(IFL),2)
C
C       (Give back W allotment)
        call WGIVE   (W,'GARDEN')
      end if
C     !END
      call BYE ('GARDEN')
C
      return
      end
