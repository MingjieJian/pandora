      subroutine MIKI
     $(IN,MUX)
C
C     Rudolf Loeser, 1973 Feb 05
C---- Allocates the Continuum Data Block.
C     !DASH
      save
C     !DASH
      integer IN, MUX, N, NN
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- OPACITY     as of 2007 Jan 12
C     Paraphernalia for background absorption/emission contributors.
C
C     (Must recompile BARE, BRACE, FORAGER & SHARI when changing NABS!)
      parameter   (NABS=45)
C
      integer     NABS,NOPAC,KOPAC,TOPAC,LOPAC
      character   CNAME*24,SYMID*1,SHNAM*6
      dimension   KOPAC(NABS),LOPAC(NABS),SYMID(NABS)
      dimension   CNAME(NABS),SHNAM(NABS)
C
      common      /OPAC1/ NOPAC
      common      /OPAC2/ KOPAC
      common      /OPAC3/ LOPAC
      common      /OPAC4/ CNAME
      common      /OPAC5/ SYMID
      common      /OPAC6/ SHNAM
C
C     NOPAC = number of contributors
C     KOPAC = contributor status switch: 0 = don't use, 1 = use
C     CNAME = name (description) of contributor
C             NOTE: This sequence of names establishes the indices or
C                   ordinals by which the contributors are also known.
C     SHNAM = "short" name of contributor
C     LOPAC = "printout order" list of contributor ordinals
C     SYMID = scratch space for symbolic identifiers
C     .
C     !EJECT
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
C     !EJECT
      external HI, BYE
C
      dimension IN(*)
C
      call HI ('MIKI')
C     !BEG
      NN = N*NOPAC
C
      IN( 1) = 1
C
      IN( 2) = IN( 1)+1
      IN( 3) = IN( 2)+1
      IN( 4) = IN( 3)+NOPAC
      IN( 5) = IN( 4)+1
      IN( 6) = IN( 5)+NCSBA
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+N
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+N
      IN(11) = IN(10)+N
C
      IN(12) = IN(11)+N
      IN(13) = IN(12)+N
      IN(14) = IN(13)+N
      IN(15) = IN(14)+N
      IN(16) = IN(15)+N
      IN(17) = IN(16)+N
      IN(18) = IN(17)+N
      IN(19) = IN(18)+1
      IN(20) = IN(19)+NN
      IN(21) = IN(20)+1
C
      IN(22) = IN(21)+NN
      IN(23) = IN(22)+N
      IN(24) = IN(23)+N
      IN(25) = IN(24)+N
      IN(26) = IN(25)+N
      IN(27) = IN(26)+N
      IN(28) = IN(27)+N
      IN(29) = IN(28)+N
      IN(30) = IN(29)+N
      IN(31) = IN(30)+N
C
      IN(32) = IN(31)+N
      IN(33) = IN(32)+1
      IN(34) = IN(33)+1
      IN(35) = IN(34)+1
      IN(36) = IN(35)+N
      IN(37) = IN(36)+N
      IN(38) = IN(37)+N
      IN(39) = IN(38)+N
      IN(40) = IN(39)+1
      IN(41) = IN(40)+100
C
      IN(42) = IN(41)+N
      IN(43) = IN(42)+1
      IN(44) = IN(43)+NOPAC
      IN(45) = IN(44)+NOPAC
      IN(46) = IN(45)+N
      IN(47) = IN(46)+1
      IN(48) = IN(47)+N
      IN(49) = IN(48)+1
      IN(50) = IN(49)+N
      IN(51) = IN(50)+N
C
      IN(52) = IN(51)+N
      IN(53) = IN(52)+N
      IN(54) = IN(53)+N
      IN(55) = IN(54)+1
      IN(56) = IN(55)+1
      IN(57) = IN(56)+NOPAC
      IN(58) = IN(57)+N
      IN(59) = IN(58)+N
      MUX    = IN(59)+N
C     !END
      call BYE ('MIKI')
C
      return
      end
