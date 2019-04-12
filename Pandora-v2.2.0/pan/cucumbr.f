      subroutine CUCUMBR
     $(IPOP,X,W)
C
C     Rudolf Loeser, 2006 Dec 29
C---- Supervises updating of C or O RABDs for molecules.
C     !DASH
      save
C     !DASH
      real*8 ABD, W, X
      integer IN, IPOP, JJCHN, JJHND, JJNCO, JJOHN, JJRAB, KODE, LU, N,
     $        NO
      character QELSM*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ( 53),JJRAB)
      equivalence (IZOQ(184),JJNCO)
      equivalence (IZOQ(268),JJCHN)
      equivalence (IZOQ(269),JJOHN)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (QZQ(  2),QELSM)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !EJECT
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
C     !DASH
      external HUPA, ZEUS, GURKE, HI, BYE
C
      dimension X(*), W(*)
C
      dimension ABD(3)
C
      call HI ('CUCUMBR')
C     !BEG
      if((IPOP.eq.2).or.(IPOP.eq.11)) then
C----   Get abundances, and element index
        call HUPA  (QELSM, ABD, IN, KODE)
        if((IN.eq.1).or.(IN.eq.2)) then
C----     Set up printout file
          call ZEUS  (NO, IPSWICH(IPOP), LU)
C----     Compute (? and print)
          call GURKE (LU, N, ABD, IN, X(JJHND), X(JJNCO), X(JJCHN),
     $                X(JJOHN), X(JJRAB))
        end if
      end if
C     !END
      call BYE ('CUCUMBR')
C
      return
      end
