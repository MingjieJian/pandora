      subroutine OOBLECK
     $(NO,SHORT,N,MF,ML,WTAB,XINT,WS,SNU,Z,IWS,ISIG,LEGEND,KODE,KOELS)
C
C     Rudolf Loeser, 1973 Dec 05
C---- Makes depth-of-formation printouts.
C     !DASH
      save
C     !DASH
      real*8 SNU, WS, WTAB, XINT, Z
      integer ISIG, IWS, KODE, KOELS, MF, ML, N, NO
      logical LEGEND, SHORT
C     !DASH
      external HOAR, SLEET, SNOW, HAIL, TYPHOON, HI, BYE
C
C               MM = KM or Nmkuse
C
C               WTAB(MM), XINT(MM), WS(N,MM), SNU(N,MM), IWS(N,MM),
      dimension WTAB(*),  XINT(*),  WS(*),    SNU(*),    IWS(*),
C
C               ISIG(N,MM), Z(N)
     $          ISIG(*),    Z(*)
C
      call HI ('OOBLECK')
C     !BEG
C---- Compute percentages (integers)
      call SLEET     (N,MF,ML,WS,SNU,XINT,IWS)
C---- Write legend, if needed
      call HOAR      (NO,SHORT,LEGEND,KODE,KOELS)
      if(SHORT) then
C----   Print abbreviated data
        call TYPHOON (NO,N,MF,ML,IWS,KOELS)
      else
C----   Determine markers
        call SNOW    (N,MF,ML,IWS,ISIG)
C----   Print detailed data
        call HAIL    (NO,Z,N,MF,ML,WTAB,IWS,ISIG,KODE,KOELS)
      end if
C     !END
      call BYE ('OOBLECK')
C
      return
      end
