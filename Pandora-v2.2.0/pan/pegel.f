      subroutine PEGEL
     $(N,TE,XNE,PGS,XION,RHEAB,HEK,HE1,HE2K,HE21,DEE,W,IW,DUMP)
C
C     Rudolf Loeser, 1990 Jul 03
C---- Supervises the calculation of the matrices DEE,
C     by the "improved" method,
C     for the diffusion calculation.
C
C     Adapted from program DIFHELIO, July 3, 1990, by
C
C     J u a n   F o n t e n l a .
C
C
C     (See DIFHELIO.FOR in directory fontenla/obsolete; writeup # 794.)
C     !DASH
      save
C     !DASH
      real*8 ABUN, ACAR, AMAS, ATON, BETA, BOLZMN, DEE, ENE, FPE, HE1,
     $       HE21, HE2K, HEABD, HEK, HEMASS, HYMASS, OIH, OIM, OIP, ONE,
     $       PART, PE, PET, PGS, PHID, RHEAB, TE, TWO, VCM, W, XIH, XIM,
     $       XION, XIP, XK, XNE, dummy
      integer I, IDEDP, IW, KODE, N, NEFDF
      logical DMPI, DUMP
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(114),IDEDP)
      equivalence (KZQ(163),NEFDF)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 2),BOLZMN)
      equivalence (PCON( 8),HYMASS)
      equivalence (PCON( 5),HEMASS)
C     !EJECT
C---- ERODIUM     as of 2007 Apr 13
      integer     IPDFON
      real*8      APDFON,APDPPR
      dimension   APDFON(9), APDPPR(6)
      common      /ERODIUM1/ IPDFON
      common      /ERODIUM2/ APDFON,APDPPR
C     Parameters for "improved" d coefficients.
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external MOTUGA, DIVIDE, MOVE1, DEEHEE, FRANK, BRECON, HALT,
     $         HI, BYE
C
      dimension W(*), IW(*)
C
C               DEE(4,5,N), HE21(N), XION(N), HEK(N), HE1(N), RHEAB(N),
      dimension DEE(4,5,*), HE21(*), XION(*), HEK(*), HE1(*), RHEAB(*),
C
C               HE2K(N), TE(N), PGS(N), XNE(N)
     $          HE2K(*), TE(*), PGS(*), XNE(*)
C
      dimension PART(5)
C
      call HI ('PEGEL')
C     !BEG
      if((NEFDF.lt.1).or.(NEFDF.gt.2)) then
        write (MSSLIN(1),100) NEFDF
  100   format('NEFDF =',I12,', which is neither 1 nor 2.')
        call HALT ('PEGEL', 1)
      end if
C
      call MOTUGA (N, IPDFON)
C---- Set up basic data - ACAR: ion charge
C                         ATON: atomic number
C                         AMAS: atomic mass
C                         XK  : Boltzmann constant, k
C                         VCM : 2*k/mass(Hydrogen)
      ACAR = ONE
      ATON = TWO
      AMAS = HEMASS
      XK   = BOLZMN
      VCM  = (TWO*XK)/HYMASS
      call FRANK  ('HE ', 0, HEABD, dummy, dummy, dummy, KODE)
C     !EJECT
      do 101 I = 1,N
        DMPI = .false.
        ABUN = HEABD*RHEAB(I)
C----   Set up partial pressures
        XIH  = XION(I)
        BETA = (HEK(I)+HE21(I))/TWO
        call DIVIDE   (BETA,    HE1(I), XIM)
        call DIVIDE   (HE2K(I), BETA,   XIP)
        call DIVIDE   (ONE,     XIH,    OIH)
        call DIVIDE   (ONE,     XIM,    OIM)
        call DIVIDE   (ONE,     XIP,    OIP)
        FPE  = XIH/(ONE+XIH)+
     $         ABUN*XIM*(ONE+TWO*XIP)/(ONE+XIM*(ONE+XIP))
        PHID = PGS(I)/(ONE+ABUN+FPE)
        PET  = PHID*ABUN
        PART(1) = PHID/(ONE+XIH)
        PART(2) = PHID/(ONE+OIH)
        PART(3) = PET/(ONE+XIM*(ONE+XIP))
        PART(4) = PET/(XIP+ONE+OIM)
        PART(5) = PET/(ONE+(ONE+OIM)*OIP)
        if(NEFDF.eq.1) then
          PE  = PART(2)+PART(4)+TWO*PART(5)
          ENE = PE/XK/TE(I)
        else
          ENE = XNE(I)
          PE  = ENE*XK*TE(I)
        end if
        if(I.eq.IPDFON) then
C----     Save for printing
          APDFON(1) = TE(I)
          APDFON(2) = PGS(I)
          APDFON(3) = BETA
          APDFON(4) = XIH
          APDFON(5) = XIM
          APDFON(6) = XIP
          APDFON(7) = ENE
          APDFON(8) = ABUN
          APDFON(9) = AMAS
          call MOVE1  (PART, 5, APDPPR)
          APDPPR(6) = PE
          DMPI      = (IDEDP.gt.0).and.DUMP
        end if
        if(DMPI) then
          call BRECON (I, IDEDP, TE(I), PGS(I), XIH, XIM, XIP, AMAS,
     $                 ABUN, XK, VCM)
        end if
        call DEEHEE   (TE(I), PART, ENE, ACAR, ATON, AMAS, ABUN, XK,
     $                 VCM, W, IW, DMPI, DEE(1,1,I))
  101 continue
C     !END
      call BYE ('PEGEL')
C
      return
      end
