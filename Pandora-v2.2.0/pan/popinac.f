      subroutine POPINAC
     $(KAMB,N,XN1,XNK,HND,H1,HK,HEND,HE1,HEK,BETA,HE21,HE2K,RHEAB,
     $ SHE,SHE2,XPBL,PNF,LU,IN1R,DUMP)
C
C     Rudolf Loeser, 1989 Sep 21
C---- Updates population extracts and related quantities, for CARAMBA.
C     !DASH
      save
C     !DASH
      real*8 BETA, H1, HE1, HE21, HE2K, HEK, HEND, HK, HND, PNF, RHEAB,
     $       SHE, SHE2, XN1, XNK, XPBL
      integer IN1R, KAMB, LLPOPN, LU, LUEO, N, NO
      logical DUMP
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
      equivalence (LZOQ( 4),LLPOPN)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !EJECT
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external MOVE1, STABEET, BRYMBO, ARRMUL, POPIO, ARRADD, ROWSUM,
     $         HALT, LINER, HI, BYE
C
C               XNK(N), XN1(N), HK(N), H1(N), HEK(N), HE21(N), BETA(N),
      dimension XNK(*), XN1(*), HK(*), H1(*), HEK(*), HE21(*), BETA(*),
C
C               HE2K(N), HE1(N), PNF(N), HEND(N), XPBL(Lenpbl), HND(N),
     $          HE2K(*), HE1(*), PNF(*), HEND(*), XPBL(*),      HND(*),
C
C               RHEAB(N), SHE(N), SHE2(N)
     $          RHEAB(*), SHE(*), SHE2(*)
C
      call HI ('POPINAC')
C     !BEG
      if((KAMB.lt.1).or.(KAMB.gt.3)) then
        write (MSSLIN(1),100) KAMB
  100   format('KAMB =',I12,', which is not 1, 2 or 3.')
        call HALT  ('POPINAC', 1)
      end if
C
      if(KAMB.eq.1) then
        call MOVE1 (XN1, N, H1  )
        call MOVE1 (XNK, N, HK  )
      else if(KAMB.eq.2) then
        call MOVE1 (XN1, N, HE1 )
        call MOVE1 (XNK, N, HEK )
      else
        call MOVE1 (XN1, N, HE21)
        call MOVE1 (XNK, N, HE2K)
      end if
C
      if((KAMB.eq.2).or.(KAMB.eq.3)) then
        call POPIO  ('ASSURE', 4, XPBL)
        call ROWSUM (XPBL(LLPOPN), N, N, 2, LENPOP(4), SHE )
        call ARRMUL (SHE, PNF, SHE, N)
        call ARRADD (HE1, SHE, SHE, N)
C
        call POPIO  ('ASSURE', 5, XPBL)
        call ROWSUM (XPBL(LLPOPN), N, N, 2, LENPOP(5), SHE2)
        call ARRMUL (SHE2, PNF , SHE2, N)
        call ARRADD (HE21, SHE2, SHE2, N)
      end if
C
      call STABEET (N, HEK, SHE2, BETA)
      call BRYMBO  (N, RHEAB, HEND)
      call ARRMUL  (HEND, HND, HEND, N)
C     !EJECT
      if(DUMP) then
        NO = LUEO
      else
        NO = LU
      end if
      if(NO.gt.0) then
        call LINER  (2, NO)
        if(IN1R.gt.0) then
          write (NO,101) IN1R
  101     format(' ','+++++ Report from POPINAC',:,', N1-iter =',I2)
        else
          write (NO,101)
        end if
        if(KAMB.eq.1) then
          write (NO,102) 'H1 and HK'
        else if(KAMB.eq.2) then
          write (NO,102) 'HE1, HEK, BETA, HEND, SHE, and SHE2'
        else
          write (NO,102) 'HE21, HE2K, BETA, HEND, SHE, and SHE2'
        end if
  102   format(' ','+++++ ',A,' have been updated.')
      end if
C     !END
      call BYE ('POPINAC')
C
      return
      end
