      subroutine WENDY
     $(A,INC,N,M,CALLER)
C
C     Rudolf Loeser, 1994 Oct 25
C---- Sets up a Continuum Recalculation control "checksum".
C     (This is version 4 of WENDY.)
C     !DASH
      save
C     !DASH
      real*8 A
      integer INC, M, N
      character CALLER*(*)
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
C     !DASH
      external TANCRED, VIOLA, HI, BYE
C
C               A(INC*(N-1)+1)
      dimension A(*)
C
      call HI ('WENDY')
C     !BEG
      call TANCRED (A,INC,N,CSBA(M))
      call VIOLA   (CALLER,M)
C     !END
      call BYE ('WENDY')
C
      return
      end
