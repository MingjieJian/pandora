      subroutine DANGUS
     $(NO,DODIDH,SVDIDH,DIDH,MYX,MUX,YY,XLTIT,WTAB,LTYPE,NW,Z,TE,N,
     $ EMU,L,W,IW)
C
C     Rudolf Loeser, 1991 Jul 05
C---- Prints/plots/saves dI/dh, at continuum wavelengths, for BUDDHA.
C     !DASH
      save
C     !DASH
      real*8 DIDH, EMU, TE, W, WTAB, XLTIT, YY, Z
      integer ILST, IN, IRUN, IS, IW, IWS, J, JLST, JN, KMX, KODE, L,
     $        LNGTH, LTYPE, MOX, MUX, MYX, N, NO, NW, jummy
      logical DETAIL, DODIDH, SVDIDH
C     !DASH
C     !EJECT
      external DOTHRA, OSTUNI, IRIEL, ILIRE, MARCY, WGIVE, IGIVE,
     $         HI, BYE
C
      dimension W(*), IW(*)
C
C               DIDH(N,Nmkuse,L), MYX(Nmkuse,L), MUX(Nmkuse,L), EMU(L),
      dimension DIDH(N,NW,*),     MYX(NW,*),     MUX(NW,*),     EMU(*),
C
C               XLTIT(Nmkuse), WTAB(Nmkuse), YY(Nmkuse,L), Z(N), TE(N),
     $          XLTIT(*),      WTAB(*),      YY(NW,*),     Z(*), TE(*),
C
C               LTYPE(Nmkuse)
     $          LTYPE(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IRUN  )
C
      dimension JN(2)
      equivalence
     $(JN( 1),ILST  ),(JN( 2),JLST  )
C
      data KODE,KMX /2, 100/
      data DETAIL   /.true./
C
      call HI ('DANGUS')
C     !BEG
      if((NO.gt.0).and.DODIDH) then
C       (Get, and allocate, W & IW allotments)
        call IRIEL      (IN, IS,  MOX, 'DANGUS')
        call ILIRE      (JN, IWS, MUX, 'DANGUS')
C
        call DOTHRA     (IW(ILST), LNGTH)
        if(LNGTH.gt.0) then
          do 100 J = 1,L
            call OSTUNI (IW(ILST), IW(JLST), LNGTH, DIDH(1,1,J),
     $                   MYX(1,J), MUX(1,J), YY(1,J), XLTIT, NW,
     $                   jummy, jummy, EMU(J), W(IRUN), Z, TE, N,
     $                   NO, WTAB, KODE, DETAIL)
  100     continue
        end if
        if(SVDIDH) then
C----     Save dI/dh in special spectrum save file, for first MU value
C         only, for first KMX Additional Wavelengths only
          call MARCY    (WTAB, LTYPE, NW, EMU(1), N, DIDH(1,1,1), KMX)
        end if
C
C       (Give back W & IW allotments)
        call WGIVE      (W,  'DANGUS')
        call IGIVE      (IW, 'DANGUS')
      end if
C     !END
      call BYE ('DANGUS')
C
      return
      end
