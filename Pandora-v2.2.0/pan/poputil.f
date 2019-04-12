      subroutine POPUTIL
     $(XPBL,NPOP, K1,XN1,K2,XNK,K3,SM1,K4,SM2)
C
C     Rudolf Loeser, 2001 Dec 13
C---- Provides subsets of data for population-ion NPOP
C     (see explanations below).
C
C     The array XN1 is filled only if K1 > 0;
C     similarly with K2,XNK,  K3,SM1,  and  K4,SM2.
C     !DASH
      save
C     !DASH
      real*8 SM1, SM2, XN1, XNK, XPBL
      integer K1, K2, K3, K4, LLPOPK, LLPOPN, N, NPOP
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
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
      equivalence (LZOQ( 4),LLPOPN)
      equivalence (LZOQ( 3),LLPOPK)
C     !DASH
C     !EJECT
      external POPIO, MOVE1, ROWSUM, ARRADD, HI, BYE
C
C               XPBL(Lenpbl), XN1(N), XNK(N), SM1(N), SM2(N)
      dimension XPBL(*),      XN1(*), XNK(*), SM1(*), SM2(*)
C
      call HI ('POPUTIL')
C     !BEG
      if((K1+K2+K3+K4).gt.0) then
C       Make sure the buffer contains the data for ion NPOP
        call POPIO      ('ASSURE', NPOP, XPBL)
C
        if(K1.gt.0) then
C
C----     Level-1 population
C
          call MOVE1    (XPBL(LLPOPN), N, XN1)
        end if
C
        if(K2.gt.0) then
C
C----     Ionized population
C
          call MOVE1    (XPBL(LLPOPK), N, XNK)
        end if
C
        if(K4.gt.0) then
C
C----     Sum of the populations of all excited levels,
C         WITHOUT the ground-state population
C
          call ROWSUM   (XPBL(LLPOPN), N, N, 2, LIMPOP(NPOP), SM2)
        end if
C
        if(K3.gt.0) then
C
C----     Sum of the populations of ALL levels =
C         total number density of this ion
C
          if(K4.gt.0) then
C           SM2 exists, just add XN1 to it
            call ARRADD (XN1, SM2, SM1, N)
          else
C           Compute from scratch
            call ROWSUM (XPBL(LLPOPN), N, N, 1, LIMPOP(NPOP), SM1)
          end if
        end if
      end if
C     !END
      call BYE ('POPUTIL')
C
      return
      end
