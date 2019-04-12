      subroutine CARINA
     $(NO,IW,N,NW,L,WAVES,WTAB,EMU,EMINT,KODE,MUX,YY,BRIGHT,YSTAR,
     $ EMINTA,BRIGHTA,YSTARA,OPACF,LTYPA,XLTIT,IJECT,LFB,MYX,
     $ AVCON,AVCONA,Z,WSSAV,SNUSAV,LEGEND,CONINT,LININT,LINK)
C
C     Rudolf Loeser, 1986 Feb 21
C---- Supervises printing of emergent continuous intensities.
C     !DASH
      save
C     !DASH
      real*8 AVCON, AVCONA, BRIGHT, BRIGHTA, EMINT, EMINTA, EMU, OPACF,
     $       SNUSAV, WAVES, WSSAV, WTAB, XLTIT, YSTAR, YSTARA, YY, Z
      integer IJECT, IW, J, KODE, L, LFB, LINK, LTYPA, MUX, MYX, N, NO,
     $        NW
      logical CONINT, INCDNT, INCRAD, LEGEND, LININT
C     !DASH
      external LORIS, DOWNY, EFT, RADULA, ABJECT, LARISA, UDRALA,
     $         HI, BYE
C
      dimension IW(*)
C
C               EMINT(Nmkuse,L), YSTAR(Nmkuse,L), YY(Nmkuse,L), EMU(L),
      dimension EMINT(NW,*),     YSTAR(NW,*),     YY(NW,*),     EMU(*),
C
C               KODE(Nmkuse,L), MUX(Nmkuse,L), MYX(Nmkuse,L), AVCON(L),
     $          KODE(NW,*),     MUX(NW,*),     MYX(NW,*),     AVCON(*),
C
C               LTYPA(Nmkuse), WSSAV(N,Nmkuse,L), EMINTA(Nmkuse), Z(N),
     $          LTYPA(*),      WSSAV(N,NW,*),     EMINTA(*),      Z(*),
C
C               BRIGHT(Nmkuse,L), BRIGHTA(Nmkuse), SNUSAV(N,Nmkuse,L),
     $          BRIGHT(NW,*),     BRIGHTA(*),      SNUSAV(N,NW,*),
C
C               YSTARA(Nmkuse), XLTIT(Nmkuse), OPACF(Nmkuse),
     $          YSTARA(*),      XLTIT(*),      OPACF(*),
C
C               WAVES(Nmkuse), WTAB(Nmkuse)
     $          WAVES(*),      WTAB(*)
C
      data INCDNT /.false./
C     !EJECT
C
      call HI ('CARINA')
C     !BEG
      if(NO.gt.0) then
C
C----   Print general header
        call LORIS        (NO, LFB, LINK, IJECT)
C
        do 100 J = 1,L
C----     Get incident-radiation-flag
          call DOWNY      (LFB, EMU(J), INCRAD)
C----     Print header for this mu value
          call EFT        (NO, EMU(J), INCDNT)
C
C----     Print intensities, etc.
          call RADULA     (NO, NW, WAVES, WTAB, KODE(1,J), MUX(1,J),
     $                     YY(1,J), OPACF, EMINT(1,J), YSTAR(1,J),
     $                     BRIGHT(1,J), LTYPA, XLTIT, MYX(1,J),
     $                     AVCON(J))
C----     Print explanations
          call UDRALA     (NO)
C----     Print depth-of-formation analysis
          call LARISA     (NO, Z, NW, WTAB, EMINT(1,J), WSSAV(1,1,J),
     $                     SNUSAV(1,1,J), LEGEND, IW)
          if((J.lt.L).or.INCRAD) then
            call ABJECT   (NO)
          end if
C
          if(INCRAD) then
C           Print results with incident radiation
            call EFT      (NO, EMU(J), INCRAD)
            call RADULA   (NO, NW, WAVES, WTAB, KODE(1,J), MUX(1,J),
     $                     YY(1,J), OPACF, EMINTA, YSTARA, BRIGHTA,
     $                     LTYPA, XLTIT, MYX(1,J), AVCONA)
C----       Print explanations
            call UDRALA   (NO)
            if(J.lt.L) then
              call ABJECT (NO)
            end if
          end if
  100   continue
C
      end if
C     !END
      call BYE ('CARINA')
C
      return
      end
