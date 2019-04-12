      subroutine BLUE
     $(NLTE,K,N,TNU,TMU,TMUN,SN,WS,YHZ,MUX,YY,KODE,WTAB,WSSAV,SNSAV,Z,
     $ DODIDH,DIDH,MYX,ZTM,W)
C
C     Rudolf Loeser, 2000 Jul 20
C---- Computes "Intensity/Hz"; i.e. YHZ as a function of WTAB, length K.
C
C---- Also saves or computes, if MUK .gt. 0:
C     MUX,YY,KODE - numerological information associated with YHZ;
C     DIDH,MYX - dI/dh and associated numerological information;
C     WSSAV,SNSAV - data for depth-of-formation analysis; and
C     ZTM  - data for depths-of-formation graph (BROOM).
C     (This is version 4 of BLUE.)
C     !DASH
      save
C     !DASH
      real*8 DIDH, SN, SNSAV, TMU, TMUN, TNU, W, WS, WSSAV, WTAB, YHZ,
     $       YHZP, YY, YYP, Z, ZTM
      integer J, K, KODE, KODEP, MUX, MUXP, MYX, MYXP, N, NLTE
      logical DODIDH, LMUK, LORI, lummy
C     !COM
C---- LOOPER      as of 2006 May 05
      integer     NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      real*8      EMOO,WVLTRN
      logical     EXPAND,SPHERE,VXZERO,FLOBRD
      common      /LOOPER1/ NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      common      /LOOPER2/ EMOO,WVLTRN
      common      /LOOPER3/ EXPAND,SPHERE,VXZERO,FLOBRD
C
C     Emergent Profiles calculations control data (simplified version).
C
C     NVEL   : number of velocity tables
C     NVY    : current value of velocity-loop index, 1.le.NVY.le.NVEL
C              (i.e. index of current velocity set)
C     JVEL   : code describing current velocity set (i.e. KVEL(NVY) )
C              =     1 : VSB
C              =     2 : VXS
C              =     3 : VADD     (from AMDIFF and/or VELGRAD)
C              = 100+j : VXN(j)
C              = 200+j : VXN(j) + VADD
C              = 300+j : VAX(j)
C              = 400+j : VFB(j)
C              = 500+j : VFB(j) + VADD
C
C     LFBV   : number of viewing positions (front only, or back also)
C     LFB    : current value of views-loop index, 1 .le. LFB .le. LFBV
C              = 1 - front-face
C              = 2 - back-face
C
C     MF     : current value of look-angles-loop index, 1.le.MF.le.LF
C     MUK    : is .gt. 0 if line intensity profile must be printed
C              [when MUK > 0, then EMOO = EMU(MUK) ]
C     EMOO   : current value of look-angle
C     WVLTRN : wavelength (Angstroms) (i.e. at Delta-Lambda = 0).
C
C     VXZERO : tells whether the current velocity =0, or not
C     EXPAND : tells whether the procedures for expanding atmospheres
C              should be used (set up in SWEET)
C     SPHERE : tells whether this is a spherical, as opposed to
C              plane-parallel, atmosphere
C     FLOBRD : tells whether to use the flow-broadening procedure
C     .
C     !DASH
      external BAUCIS, SIMBA, MOVE1, DANZIG, TANG, PINK, HI, BYE
C
      dimension W(*)
C
C               TNU(N,KM), TMU(N), TMUN(KM), YHZ(KM), MUX(KM), MYX(KM),
      dimension TNU(N,*),  TMU(*), TMUN(*),  YHZ(*),  MUX(*),  MYX(*),
C
C               KODE(KM), SN(N,KM), WTAB(KM), WS(N), Z(N), WSSAV(N,KM),
     $          KODE(*),  SN(N,*),  WTAB(*),  WS(*), Z(*), WSSAV(N,*),
C
C               SNSAV(N,KM), DIDH(N,KM), ZTM(KM,4), YY(KM)
     $          SNSAV(N,*),  DIDH(N,*),  ZTM(*),    YY(*)
C     !EJECT
C
      call HI ('BLUE')
C     !BEG
      LORI = (NLTE.gt.0)
      LMUK = (MUK.gt.0)
C
C---- Loop over all line profile wavelengths
      do 100 J = 1,K
C
C----   Compute optical depth
        call TANG      (EMOO,1,TNU(1,J),N,TMU)
C----   Compute intensity /Hz
        call SIMBA     (LFB,WTAB(J),N,TMU,SN(1,J),WS,YHZP,MUXP,YYP,
     $                  KODEP)
C----   Compute dI/dh, and index of its maximum value
        call DANZIG    (DODIDH,N,Z,WS,SN(1,J),DIDH(1,J),MYXP,W)
C
C----   Save results
        YHZ(J) = YHZP
C
        if(LMUK) then
          MUX(J)  = MUXP
          YY(J)   = YYP
          MYX(J)  = MYXP
          KODE(J) = KODEP
          TMUN(J) = TMU(N)
C----     Provide debug printout
          call PINK    (WTAB(J),EMOO,YHZP,MUXP,MYXP,YYP,KODEP,TNU(1,J),
     $                  TMU,SN(1,J),WS,N,LFB,lummy)
          if(LORI) then
C----       Save depth-of-formation information
            call MOVE1 (WS     ,N,WSSAV(1,J))
            call MOVE1 (SN(1,J),N,SNSAV(1,J))
          end if
          call BAUCIS  (J,K,MUXP,TMU,Z,N,ZTM)
        end if
C
  100 continue
C     !END
      call BYE ('BLUE')
C
      return
      end
