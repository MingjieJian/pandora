      subroutine ARRAS
     $(W,IW,N,NW,MUXX,BRIGHT,Y,XLAM,YNT,XLTIT,ISTAR,MODE,MYXX, ISIG)
C
C     Rudolf Loeser, 1973 Apr 04
C---- "Sorts" Spectrum Summary data.
C     !DASH
      save
C     !DASH
      real*8 BRIGHT, W, XLAM, XLTIT, Y, YNT
      integer ICOMK, IN, IPNT, IS, ISIG, ISTAR, IVEC1, IVEC2, IW, IWS,
     $        JN, MODE, MOX, MUX, MUXX, MYXX, N, NW
C     !DASH
      external RASAR, LITHON, THOLIN, SINGD, ORDERD, ORDERI, WGIVE,
     $         IGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               MODE(NW), BRIGHT(NW), XLTIT(NW), ISTAR(NW), XLAM(NW),
      dimension MODE(*),  BRIGHT(*),  XLTIT(*),  ISTAR(*),  XLAM(*),
C
C               Y(NW), YNT(NW), MYXX(NW), MUXX(NW)
     $          Y(*),  YNT(*),  MYXX(*),  MUXX(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),ICOMK ),(IN( 2),IVEC1 )
C
      dimension JN(2)
      equivalence
     $(IN( 1),IPNT  ),(IN( 2),IVEC2 )
C     !EJECT
C
      call HI ('ARRAS')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call LITHON   (IN,IS ,MOX,'ARRAS')
      call THOLIN   (JN,IWS,MUX,'ARRAS')
C
C---- Construct sort keys, . . .
      call RASAR    (N,NW,MUXX,BRIGHT,W(ICOMK))
C---- . . . and sort them
      call SINGD    (W(ICOMK),NW,ISIG,IW(IPNT))
      if(ISIG.gt.0) then
C----   Permute associated arrays into "sorted" order
        call ORDERD (BRIGHT,IW(IPNT),NW,W (IVEC1))
        call ORDERD (XLTIT ,IW(IPNT),NW,W (IVEC1))
        call ORDERD (XLAM  ,IW(IPNT),NW,W (IVEC1))
        call ORDERD (YNT   ,IW(IPNT),NW,W (IVEC1))
        call ORDERD (Y     ,IW(IPNT),NW,W (IVEC1))
        call ORDERI (ISTAR ,IW(IPNT),NW,IW(IVEC2))
        call ORDERI (MODE  ,IW(IPNT),NW,IW(IVEC2))
        call ORDERI (MUXX  ,IW(IPNT),NW,IW(IVEC2))
        call ORDERI (MYXX  ,IW(IPNT),NW,IW(IVEC2))
      end if
C
C     (Give back W & IW allotments)
      call WGIVE    (W ,'ARRAS')
      call IGIVE    (IW,'ARRAS')
C     !END
      call BYE ('ARRAS')
C
      return
      end
