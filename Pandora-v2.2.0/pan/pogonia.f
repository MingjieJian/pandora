      subroutine POGONIA
     $(POPK,HN1,W)
C
C     Rudolf Loeser, 1990 Oct 01
C---- Retrieves Hydrogen Level-1 number density,
C     and
C     ionized population-ions populations (except H [1] and He-II [5])
C     if needed for "ion broadening" calculation (Hydrogen only).
C     (This is version 2 of POGONIA.)
C     !DASH
      save
C     !DASH
      real*8 HN1, POPK, W, dummy
      integer IN, IS, IXPBL, J, JIBR, MOX, N, jummy
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
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(51),JIBR )
C     !DASH
      external NUTHIN, ZERO1, POPUTIL, POPIO, WGIVE, HI, BYE
C
      dimension W(*)
C
C               POPK(N,NPOPS), HN1(N)
      dimension POPK(N,*),     HN1(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IXPBL )
C
      call HI ('POGONIA')
C     !BEG
C     (Get W allotment)
      call NUTHIN        (IN, IS, MOX, 'POGONIA')
C     (Initialize populations buffer)
      call POPIO         ('INIT', jummy, W(IXPBL))
C
      call POPUTIL       (W(IXPBL), 1, 1, HN1, 0, dummy, 0, dummy,
     $                    0, dummy)
C
      call ZERO1         (POPK, (N*NPOPS))
      if(JIBR.gt.0) then
        do 100 J = 1,NPOPS
          if((J.ne.1).and.(J.ne.5)) then
            call POPUTIL (W(IXPBL), J, 0, dummy, 1, POPK(1,J), 0, dummy,
     $                    0, dummy)
          end if
  100   continue
      end if
C
C     (Give back W allotment)
      call WGIVE         (W, 'POGONIA')
C     !END
      call BYE ('POGONIA')
C
      return
      end
