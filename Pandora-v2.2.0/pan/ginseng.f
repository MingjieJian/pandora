      subroutine GINSENG
     $(XLM,TE,HN,BDH,LEVEL,N,ORES,OREM)
C
C     Rudolf Loeser, 1974 May 24
C---- Computes a set of Hydrogen bound-free opacity values.
C     (This is version 4 of GINSENG.)
C     !DASH
      save
C     !DASH
      real*8 BDH, CON58, CON60, F, FF, HN, ONE, OREM, ORES, REM, REMSUM,
     $       RES, T, TE, TINC, XLM, XX, ZERO
      integer I, J, JHBFD, LEVEL, LIMP, N
      logical DMPI, DUMP, KILROY
C     !COM
C---- POPDATA     as of 2007 Jan 12
      integer     NPI
      parameter   (NPI=14)
C     (Remember to recompile all users when changing NPI.)
      real*8      POPMSS
      integer     LZOQ,MRTP,NPOPS,MAXPOPL,LENPBL,MRTPA,MRTPM,LIMPOP,
     $            LENT,NAMKNT,LENPOP,ICKSM,IUPOP,IBLAD,IPSWICH,KAPNO
      character   NAMES*10,TNAMES*8,POPSYM*3,KLABPI*8,NLABPI*8,BLABPI*8
      dimension   LZOQ(5), MRTP(50),
     $            LIMPOP(NPI), NAMKNT(NPI), LENPOP(NPI), IBLAD(NPI),
     $            ICKSM(NPI),  IUPOP(NPI),  NAMES(NPI),  IPSWICH(NPI),
     $            POPSYM(NPI), KAPNO(NPI),  POPMSS(NPI), TNAMES(NPI),
     $            KLABPI(NPI), NLABPI(NPI), BLABPI(NPI)
C
      common      /POPS01/ NPOPS,MAXPOPL,LENT,LENPBL,MRTPM,MRTPA,ICKSM
      common      /POPS02/ POPMSS
      common      /POPS03/ LZOQ
      common      /POPS04/ MRTP
      common      /POPS05/ LENPOP
      common      /POPS06/ LIMPOP
      common      /POPS07/ NAMES
      common      /POPS08/ TNAMES
      common      /POPS09/ NAMKNT
      common      /POPS10/ IUPOP
      common      /POPS11/ IBLAD
      common      /POPS12/ IPSWICH
      common      /POPS13/ POPSYM
      common      /POPS14/ KAPNO
      common      /POPS15/ KLABPI
      common      /POPS16/ NLABPI
      common      /POPS17/ BLABPI
C
C     Population Data Blocks parameters and data.
C     !EJECT
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(149),JHBFD)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external RIGEL, FLASH, JIMSON, MINNA, GLORY, GLORA, GLORO, YALTA,
     $         HI, BYE, VECOUT
C
C               HN(N,LIMP), BDH(N,LIMP), ORES(N), OREM(N), TE(N)
      dimension HN(N,*),    BDH(N,*),    ORES(*), OREM(*), TE(*)
C
      call HI ('GINSENG')
C     !BEG
      call RIGEL (58, CON58)
      call RIGEL (60, CON60)
C
      FF     = CON60*(XLM**3)
      LIMP   = LIMPOP(1)
      XX     = 2*(LIMP**2)
      REMSUM = ONE/(XX*CON58)
      call GLORY (DUMP, 'GINSENG', XLM, LIMP, FF, REMSUM)
C     !EJECT
      do 101 I = 1,N
        call MINNA     (DUMP, I, JHBFD, DMPI)
C
        RES    = ZERO
        REM    = ZERO
        KILROY = .true.
        do 100 J = 1,LIMP
          call JIMSON  (J, XLM, TE(I), HN(I,J), BDH(I,J), T)
          if(J.eq.LIMP) then
            call FLASH (TE(I), XLM, LIMP, F)
            TINC = HN(I,J)*TE(I)*F*REMSUM
          else
            F    = ZERO
            TINC = ZERO
          end if
          T = T+TINC
          if(J.eq.LEVEL) then
            RES = T
          else
            REM = REM+T
          end if
C
          if(DMPI) then
            call GLORA (KILROY, I, TE(I), J, HN(I,J), BDH(I,J), F,
     $                  TINC, T, RES, REM)
          end if
  100   continue
C
        ORES(I) = FF*RES
        OREM(I) = FF*REM
        if(DMPI) then
          call GLORO   (I, ORES(I), OREM(I))
        end if
  101 continue
C
      call YALTA       (DUMP, 'GINSENG')
C     !END
      call BYE ('GINSENG')
C
      return
      end
