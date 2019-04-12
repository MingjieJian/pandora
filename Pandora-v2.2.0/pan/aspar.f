      subroutine ASPAR
     $(NDT,XLDT,N,OPAC,TAU,S,XJNU,CNXP,XCBL)
C
C     Rudolf Loeser, 1973 Oct 24
C---- Reads continuum data, for QUIVER.
C     (This is version 2 of ASPAR.)
C     !DASH
      save
C     !DASH
      real*8 CNXP, OPAC, S, TAU, XCBL, XJNU, XLDT
      integer ITYPE, J, KKCNXP, KKJNU, KKOPAC, KKSCON, KKTAUK, N, NDT
C     !COM
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(13),KKJNU )
      equivalence (KKK( 6),KKOPAC)
      equivalence (KKK(12),KKTAUK)
      equivalence (KKK(14),KKSCON)
      equivalence (KKK(11),KKCNXP)
C     !DASH
      external LAMAR, MOVE1, HI, BYE
C
C               XLDT(NDT), XCBL(Miklen), XJNU(N,NDT), CNXP(N,NDT),
      dimension XLDT(*),   XCBL(*),      XJNU(N,*),   CNXP(N,*),
C
C               S(N,NDT), OPAC(N,NDT), TAU(N,NDT)
     $          S(N,*),   OPAC(N,*),   TAU(N,*)
C
      data ITYPE /8/
C
      call HI ('ASPAR')
C     !BEG
C---- Loop over all dust wavelengths
      do 100 J = 1,NDT
C----   Read continuum block
        call LAMAR (XLDT(J),ITYPE,XCBL)
C----   Copy data
        call MOVE1 (XCBL(KKOPAC),N,OPAC(1,J))
        call MOVE1 (XCBL(KKTAUK),N,TAU (1,J))
        call MOVE1 (XCBL(KKSCON),N,S   (1,J))
        call MOVE1 (XCBL(KKJNU ),N,XJNU(1,J))
        call MOVE1 (XCBL(KKCNXP),N,CNXP(1,J))
  100 continue
C     !END
      call BYE ('ASPAR')
C
      return
      end
