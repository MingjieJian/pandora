      subroutine NAOMI
     $(XLM,TE,B,BDH,LEVEL,N,SRES,SREM)
C
C     Rudolf Loeser, 1973 Sep 04
C---- Computes a set of Hydrogen
C     bound-free continuum source function terms.
C
C     N O T E : this procedure has LEVEL=1 built in;
C               thus it only works for that case.
C
C     (This is version 3 of NAOMI.)
C     !DASH
      save
C     !DASH
      real*8 B, BDH, CON58, CON6, E, EL, EN, FAC, FS, GBFL, H, HALF, HD,
     $       HN, HNUKT, HR, OL2, ON2, ONE, SE, SREM, SRES, TE, U, XL2,
     $       XL3, XLM, ZERO
      integer I, JHBFD, L, LEV, LEVEL, LIMP, N, NLV
      logical DMPI, DORES, DUMP, KILROY
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
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  NIPPY, RIGEL, YALTA, GUNNAR, QEXP1, STORY, MINNA, PROD,
     $          STORU, STORA, STORO, HI, BYE
      intrinsic max
C
C               BDH(N,NLH), TE(N), B(N), SRES(N), SREM(N)
      dimension BDH(N,*),   TE(*), B(*), SRES(*), SREM(*)
C
      call HI ('NAOMI')
C     !BEG
      call RIGEL (58, CON58)
      call RIGEL ( 6, CON6)
      call NIPPY (XLM, LEV)
      DORES = LEV.eq.1
      LIMP  = LIMPOP(1)
      NLV   = max(LEV,2)
      XL2   = NLV**2
      ON2   = ONE/XL2
      FAC   = CON6/(XLM**3)
      call STORY (DUMP, 'NAOMI', XLM, LIMP, LEV, FAC)
C     !EJECT
      do 101 I = 1,N
        call MINNA       (DUMP, I, JHBFD, DMPI)
C
        call PROD        (TE(I), XLM, 2, HNUKT, SE)
        if(DORES) then
          SRES(I) = FAC*(SE/(BDH(I,1)-SE))
          if(DMPI) then
            call STORA   (I, TE(I), 1, BDH(I,1), HNUKT, SE, SRES(I))
          end if
        else
          SRES(I) = ZERO
        end if
C
        U  = CON58/TE(I)
        HN = ZERO
        HD = ZERO
C
        if(NLV.le.LIMP) then
          KILROY = .true.
          do 100 L = NLV,LIMP
            call GUNNAR  (L, XLM, GBFL)
            XL2 = L**2
            OL2 = ONE/XL2
            XL3 = L**3
            E  = exp(-(U*(ON2-OL2)))
            H  = (E/XL3)*GBFL
            HN = HN+H
            HD = HD+H*(BDH(I,L)-SE)
            if(DMPI) then
              call STORO (KILROY, L, BDH(I,L), GBFL, E, H, HN, HD)
            end if
  100     continue
        end if
C
        call QEXP1       (HNUKT, SE, 1, FS)
        L   = max(NLV,LIMP+1)
        XL2 = L**2
        OL2 = ONE/XL2
        EL  = exp(-(U*(ON2-OL2)))
        EN  = exp(-(U* ON2))
C
        HR = HALF*(EL-EN)/U
        SREM(I) = B(I)*(FS*(HN+HR))/(HD+FS*HR)
C
        if(DMPI) then
          call STORU     (FS, EL, EN, HR, SREM(I))
        end if
  101 continue
C
      call YALTA         (DUMP, 'NAOMI')
C     !END
      call BYE ('NAOMI')
C
      return
      end
