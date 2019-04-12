      subroutine HOSEAH
     $(N,XNU,TE,XNE,POPK,RM,VION,NO,W)
C
C     Rudolf Loeser, 1990 Oct 01
C---- Calculates W for "ion broadening" of Hydrogen lines.
C
C     Derived with the help of Ed Chang (UMass) from
C     Hoang-Binh, D., Brault, P., Picart, J., Tran-Minh, N.,
C     and Vallee, O. 1987, "Ion collisional broadening of
C     solar lines in the far-infrared and submillimeter region",
C     Astron.Astrophys., 181, 134-137.
C
C     (This is version 2 of HOSEAH.)
C     !DASH
      save
C     !DASH
      real*8 C1, C2, CON10, FLU, HALF, ONE, PI, POPK, POV, RAT, RATL,
     $       RHOMAX, RHOMIN, RM, RTFLU, RULM, SUM, T2, T3, T4, TE, TERM,
     $       VION, W, XNE, XNU, ZERO
      integer I, IBRDP, IL, IU, J, JIBR, LUEO, N, NO
      logical DMPI, DUMP
C     !COM
C---- LINUS       as of 2004 May 12
      integer     LINKDS
      dimension   LINKDS(22)
      common      /LINUS/ LINKDS
C     Line source function calculation control parameters for the
C     current transition as set up by "PET" (and printed by "LINSEED").
C     IU    - index of upper level
C     IL    - index of lower level
C     KLIN  - line "type" code (1: radiative, 2: passive, etc)
C     ICE   - PRD calculation control
C     IPRO  - emergent profiles calculation control
C     METSE - statistical equilibrium calculation method selector
C     METSF - LSF calculation method selector (QR, RT, GR)
C     IBRSW - damping components selector
C     INKSW - input opacity signal
C     LSFT  - LSF solution code (0: full, 1:direct, etc)
C     ILFLX - line flux calculation control
C     LDL   - number of line components
C     LINT  - frequency integration range (half vs. full profile)
C     LSFP  - LSF printout control
C     IFDB  - LSF background control (constant vs. varying)
C     ISBG  - blended line profile plot mode switch
C     KBT   - length of input table XIBLUT
C     KRT   - length of input table XIREDT
C     KST   - length of input table XISYMT
C     KTRN  - length of actual tables XI and DL
C     LOML  - "line-background-continuum-opacity" control
C     ....  - (available)
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
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
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(115),IBRDP)
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
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 1),PI    )
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
      external  JOSIAH, JOSHUA, DIVIDE, ZELTENE, LINER, EDOUARD, EDGAR,
     $          ZERO1, RIGEL, FULUM, MASHED, HI, BYE
      intrinsic max
C
C               XNU(NSL), TE(N), POPK(N,NPOPS), RM(NPOPS), VION(NPOPS),
      dimension XNU(*),   TE(*), POPK(N,*),     RM(*),     VION(*),
C
C               XNE(N), W(N)
     $          XNE(*), W(*)
C
      data C1,C2 / 6.666666666666667D-1, 1.333333333333333D0 /
C
      call HI ('HOSEAH')
C     !BEG
      call ZERO1     (W, N)
C
      if((JIBR.gt.0).and.(IL.ge.5)) then
C----   Set up dump
        call JOSIAH  (IU, IL, NO, 'HOSEAH', DUMP)
C
C----   Set up population-ion mass-ratios (and He mass)
        call EDOUARD (POPMSS, RM, NPOPS)
C----   Calculate F(l,u)
        call JOSHUA  (IU, IL, FLU, DUMP)
        RTFLU = sqrt(C1*FLU)
C----   Constant factors
        call RIGEL   (10, CON10)
        T2 = C2*PI*(CON10**2)
        T3 = CON10*RTFLU
        T4 = T2*FLU
C     !EJECT
C----   Loop over all depths
        do 104 I = 1,N
C----     Set up dump for selected depths
          call ZELTENE    (DUMP, I, DMPI)
C----     Compute ion velocities
          call EDGAR      (TE(I), POPMSS, RM, POPSYM, NPOPS, VION,
     $                     DMPI)
C----     Compute RHOMAX
          call FULUM      (TE(I), XNE(I), XNU(IU), XNU(IL), RHOMAX,
     $                     DMPI)
C
          if(DMPI) then
            call LINER    (1, LUEO)
            write (LUEO,100)
  100       format(' ','RHOMIN, RULM, NK and sum-term for:')
          end if
C----     Compute RHOMIN, and sum of ion terms
          SUM = ZERO
          do 102 J = 1,NPOPS
            if((J.ne.1).and.(J.ne.5)) then
              call DIVIDE (POPK(I,J), VION(J), POV)
              RHOMIN = T3/VION(J)
              call DIVIDE (RHOMAX, RHOMIN, RAT)
              RATL = log(max(ONE,RAT))
              RULM = HALF+RATL
              TERM = POV*RULM
              if(DMPI) then
                write (LUEO,101) POPSYM(J),RHOMIN,RULM,POPK(I,J),TERM
  101           format(' ',A3,1P4E14.6)
              end if
            else
              TERM = ZERO
            end if
            SUM = SUM+TERM
  102     continue
C
          W(I) = T4*SUM
          if(DMPI) then
            call LINER    (1, LUEO)
            write (LUEO,103) I,W(I)
  103       format(' ','W(',I3,') =',1PE14.6)
          end if
  104   continue
C
        if(DUMP) then
          call MASHED     ('HOSEAH')
        end if
      end if
C     !END
      call BYE ('HOSEAH')
C
      return
      end
