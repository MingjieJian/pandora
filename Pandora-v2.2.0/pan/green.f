      subroutine GREEN
     $(Z,GTN,N,PHI,COPTRN,TNU,SNU,STRN,BCTRN,IU,IL,J,W,IMG,XKPNU,K,DL,
     $ EMU,WVL,VXA,SINT,FINT,GTU,DW,DWU,XKPL)
C
C     Rudolf Loeser, 1969 Sep 25
C---- Computes TNU and SNU, for DESPINA.
C     !DASH
      save
C     !DASH
      real*8 BCTRN, COPTRN, DL, DW, DWU, EMU, FINT, GTN, GTU, PHI, SINT,
     $       SNU, STRN, TNU, VXA, W, WVL, XKPL, XKPNU, Z, dummy
      integer IL, IMG, IU, J, JNK, K, N, jummy
      logical lummy1, lummy2
      character LABEL*100
C     !DASH
      external RISK, ELSI, SOUFFLE, DANK, RUMBLE, TURKANA, HOPS,
     $         HI, BYE
C
      dimension W(*)
C
C               COPTRN(N), GTU(N), STRN(N,KM), XKPL(N), DL(KM), GTN(N),
      dimension COPTRN(*), GTU(*), STRN(*),    XKPL(*), DL(*),  GTN(*),
C
C               PHI(N), TNU(N), SNU(N), IMG(N), XKPNU(N), Z(N), DWU(N),
     $          PHI(*), TNU(*), SNU(*), IMG(*), XKPNU(*), Z(*), DWU(*),
C
C               VXA(N), SINT(N), FINT(N), BCTRN(N), DW(N)
     $          VXA(*), SINT(*), FINT(*), BCTRN(*), DW(*)
C
      call HI ('GREEN')
C     !BEG
C---- Select S, apply frequency-shift if needed, and store in SINT
      call HOPS     (J,K,DL,STRN,EMU,WVL,VXA,dummy,dummy,'PLANE',jummy,
     $               W,SINT)
C---- Apply R**2-enhancement (if needed)
      call RISK     (SINT,N,Z,SINT)
C---- Set up GTN-Used (possibly modified by DW-ratio)
      call TURKANA  (N,DW,DWU,GTN,GTU)
C---- Compute total opacity, and monochromatic source term
      call ELSI     (1,N,PHI,COPTRN,GTU,XKPL,XKPNU)
      call SOUFFLE  (1,N,COPTRN,GTU,PHI,BCTRN,SINT,XKPNU,SNU)
C---- Compute monochromatic TAU
      write (LABEL,100) IU,IL,J,DL(J)
  100 format(' Monochromatic TAU, Transition',I3,'/',I2,', at Delta-',
     $       'Lambda(',I3,') =',1PE14.6)
      call DANK     (1,N,XKPNU,Z,FINT,TNU,LABEL,JNK,lummy1,lummy2,IMG,W)
      if(JNK.gt.0) then
C----   Print error message and abort
        call RUMBLE (LABEL,DL(J),JNK,TNU,N)
      end if
C     !END
      call BYE ('GREEN')
C
      return
      end
