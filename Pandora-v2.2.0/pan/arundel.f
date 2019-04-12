      subroutine ARUNDEL
     $(NO,KFUNC,RES,WTAB,K,KTC,YY,MUX,MYX,KODE,FIN,FIS,FAN,FHZ,BT,
     $ TF,SF,KNLTE)
C     Rudolf Loeser, 1986 Feb 20
C---- Prints data, for FABLE.
C     (This is version 2 of ARUNDEL.)
C     !DASH
      save
C     !DASH
      real*8 BT, FAN, FHZ, FIN, FIS, FLAG, RES, RS, SF, SHL, TF, THSND,
     $       WTAB, YY, ZERO
      integer I, K, KFUNC, KNLTE, KODE, KTC, MUX, MYX, NF, NO
      logical INTENS, KILROY
      character LINE*127
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
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  TISRIT, NIT, ALARSK, SHIM, ENCODED, LINER, SMUDGE,
     $          HI, BYE
      intrinsic abs, sign
C
C               KODE(KM), FHZ(KM), WTAB(KM), MUX(KM), MYX(KM), FIS(KM),
      dimension KODE(*),  FHZ(*),  WTAB(*),  MUX(*),  MYX(*),  FIS(*),
C
C               FIN(KM), FAN(KM), RES(KM), TF(KM), SF(KM), YY(KM),
     $          FIN(*),  FAN(*),  RES(*),  TF(*),  SF(*),  YY(*),
C
C               BT(KM)
     $          BT(*)
C
      data THSND,FLAG /1.D3, 999.999D0/
C
      call HI ('ARUNDEL')
C     !BEG
      KILROY = .true.
      INTENS = KFUNC.eq.1
C
      do 106 I = 1,K
C
        write (LINE,100) I
  100   format(I4,123X)
C
        call ENCODED    (WTAB(I), LINE(7:18), 12, 12, 1, NF)
C
        if(INTENS) then
          call TISRIT   (MUX(I), YY(I), MYX(I), LINE(23:35))
          call NIT      (KILROY, KODE(I),       LINE(36:36))
        else
          if(SPHERE) then
            call ALARSK (SPHERE, SF(I), TF(I), SHL)
            if(SHL.ne.ZERO) then
              write (LINE(25:36),101) SHL
  101         format(F12.5)
            end if
          end if
        end if
C
        if(FAN(I).ne.ZERO) then
          write (LINE(37:52),102) FAN(I)
  102     format(1PE16.7)
        end if
        write (LINE(53:68),102) FHZ(I)
C     !EJECT
        if(abs(RES(I)).lt.THSND) then
          RS = RES(I)
        else
          RS = sign(FLAG,RES(I))
        end if
        if(RS.ne.ZERO) then
          write (LINE(69:79),103) RS
  103     format(3X,F8.3)
        end if
C
        if(FIN(I).ne.ZERO) then
          write (LINE( 83: 97),104) FIN(I)
  104     format(1PE15.6)
        end if
        if(FIS(I).ne.ZERO) then
          write (LINE( 98:112),104) FIS(I)
        end if
        if(BT(I) .ne.ZERO) then
          write (LINE(113:127),104) BT(I)
        end if
C
        write (NO,105) LINE
  105   format(' ',A127)
        call SHIM   (I, 5, NO)
C
  106 continue
C
C
C
      if(INTENS) then
C----   Print error messages, if any
        call SMUDGE (NO, 36, LINE)
      end if
C     !END
      call BYE ('ARUNDEL')
C
      return
      end
