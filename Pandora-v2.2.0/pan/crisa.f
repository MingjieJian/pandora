      subroutine CRISA
     $(K,L,TCINT,TCINTA,TCFLX,BCIZ,BCZA,BCFZ,BCZB,BZAB,BFZB)
C
C     Rudolf Loeser, 1992 May 11
C---- Sets up profile background data /Hz.
C     (This is version 3 of CRISA.)
C     !DASH
      save
C     !DASH
      real*8 BCFZ, BCIZ, BCZA, BCZB, BFZB, BZAB, ONE, TCFLX, TCINT,
     $       TCINTA
      integer K, L
      logical INCRAD
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
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DOWNY, SALT, HI, BYE
C
C               TCINT(KM,L,LFBV), TCINTA(KM,LFBV), TCLFX(KM,LFBV),
      dimension TCINT(K,L,*),     TCINTA(K,*),     TCFLX(K,*),
C
C               BCIZ(KM,L), BCZA(KM), BCFZ(KM), BZAB(KM), BFZB(KM),
     $          BCIZ(*),    BCZA(*),  BCFZ(*),  BZAB(*),  BFZB(*),
C
C               BCZB(KM,L)
     $          BCZB(*)
C
      call HI ('CRISA')
C     !BEG
C---- Front face
      call DOWNY   (1, ONE, INCRAD)
      call SALT    (K, L, INCRAD, TCINT(1,1,1), TCINTA(1,1), TCFLX(1,1),
     $              BCIZ, BCZA, BCFZ)
C
      if(LFBV.eq.2) then
C----   Back face
        call DOWNY (2, ONE, INCRAD)
        call SALT  (K, L, INCRAD, TCINT(1,1,2), TCINTA(1,2), TCFLX(1,2),
     $              BCZB, BZAB, BFZB)
      end if
C     !END
      call BYE ('CRISA')
C
      return
      end
