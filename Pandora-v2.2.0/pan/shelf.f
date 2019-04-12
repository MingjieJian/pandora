      subroutine SHELF
     $(XCBL,KTRU,IIFLAG,NOPAC,ISWA,ISWE)
C
C     Rudolf Loeser, 2004 Sep 01
C---- Moves miscellaneous items into Continuum Data blocks.
C     (This is version 2 of SHELF.)
C     !DASH
      save
C     !DASH
      real*8 XCBL
      integer IIFLAG, ISWA, ISWE, KKACTO, KKCKSM, KKISWA, KKISWE,
     $        KKKODE, KKLAMD, KKLAMP, KTRU, NOPAC
C     !COM
C---- SENNA       as of 2007 Jan 12
      parameter   (LCSBA=54)
      real*8      CSBA,CSBO
      integer     LCSBA,NCSBA
      character   LABSBA*16
      dimension   CSBA(LCSBA),CSBO(LCSBA),LABSBA(LCSBA)
      common      /SENNA1/ NCSBA
      common      /SENNA2/ CSBA
      common      /SENNA3/ LABSBA
      common      /SENNA4/ CSBO
C     Checksums of quantities used to compute the various components of
C     the background absorption and the background emisssion.
C     These checksums         M  U  S  T        be updated whenever the
C              corresponding quantities are recomputed.
C
C        1 TE                 2 V                  3 XNE
C        4 HND                5 BDHM               6 TDUST
C        7 H2N                8 CON
C        9 H(nlev)           10 H(bd)             11 H(nk)
C       12 He-I(nlev)        13 He-I(bd)          14 He-I(nk)
C       15 HE-II(nlev)       16 He-II(bd)         17 He-II(nk)
C       18 C-I(nlev)         19 C-I(bd)           20 C-I(nk)
C       21 Si-I(nlev)        22 Si-I(bd)          23 Si-I(nk)
C       24 Al-I(nlev)        25 Al-I(bd)          26 Al-I(nk)
C       27 Mg-I(nlev)        28 Mg-I(bd)          29 Mg-I(nk)
C       30 Fe-I(nlev)        31 Fe-I(bd)          32 Fe-I(nk)
C       33 Na-I(nlev)        34 Na-I(bd)          35 Na-I(nk)
C       36 Ca-I(nlev)        37 Ca-I(bd)          38 Ca-I(nk)
C       39 VM
C       40 O-I(nlev)         41 O-I(bd)           42 O-I(nk)
C       43 H1
C       44 S-I(nlev)         45 S-I(bd)           46 S-I(nk)
C       47 CHN               48 OHN
C       49 O-II(nlev)        50 O-II(bd)          51 O-II(nk)
C       52 O-III(nlev)       53 O-III(bd)         54 O-III(nk)
C     .
C     !EJECT
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(18),KKACTO)
      equivalence (KKK(55),KKLAMP)
      equivalence (KKK(54),KKLAMD)
      equivalence (KKK(43),KKISWA)
      equivalence (KKK(44),KKISWE)
      equivalence (KKK( 5),KKCKSM)
      equivalence (KKK(48),KKKODE)
C     !DASH
      external MOVE1, CONTID, HI, BYE
C
C               XCBL(Miklen), ISWA(Nopac), ISWE(Nopac)
      dimension XCBL(*),      ISWA(*),     ISWE(*)
C
      call HI ('SHELF')
C     !BEG
      XCBL(KKLAMP) = XCBL(KKLAMD)
      XCBL(KKACTO) = IIFLAG
      XCBL(KKKODE) = KTRU
C
      call MOVE1  (CSBA, NCSBA, XCBL(KKCKSM))
      call CONTID (ISWA, 1, NOPAC, XCBL(KKISWA), 1, NOPAC)
      call CONTID (ISWE, 1, NOPAC, XCBL(KKISWE), 1, NOPAC)
C     !END
      call BYE ('SHELF')
C
      return
      end
