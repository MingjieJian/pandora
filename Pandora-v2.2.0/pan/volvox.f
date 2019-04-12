      subroutine VOLVOX
     $(NW,L,WAVES,LTYPE,XLTIT,EMU,BRIGHT,EMINT,YSTAR,BRIGHTA,EMINTA,
     $ YSTARA,XLB3,LFB)
C
C     Rudolf Loeser, 1986 Mar 07
C---- Saves line-free emergent continuum intensity data.
C     (See also NARKE).
C     !DASH
      save
C     !DASH
      real*8 BRIGHT, BRIGHTA, EMINT, EMINTA, EMU, WAVES, XLB3, XLTIT,
     $       YSTAR, YSTARA
      integer I, IL, IPEX, IU, KAK1, KAK2, KAK3, KIND, KTRN, L, LFB,
     $        LTYPE, LUEO, MMAAB, MMBTA, MMBTI, MMCAA, MMCAB, MMCIA,
     $        MMCIZ, MMCZA, MMCZB, MMTAB, MMTIB, MMZAB, NW
      logical GOOD
C     !COM
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MMT( 2),MMCIZ)
      equivalence (MMT( 3),MMCIA)
      equivalence (MMT( 6),MMBTI)
      equivalence (MMT( 8),MMCZA)
      equivalence (MMT( 9),MMCAA)
      equivalence (MMT(10),MMBTA)
      equivalence (MMT(11),MMCZB)
      equivalence (MMT(12),MMCAB)
      equivalence (MMT(15),MMTIB)
      equivalence (MMT(17),MMZAB)
      equivalence (MMT(18),MMAAB)
      equivalence (MMT(19),MMTAB)
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
      equivalence (KZQ( 18),IPEX )
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
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
      equivalence (LINKDS(20),KTRN )
C
C---- DWARF       as of 1997 Nov 19
      integer     KAKOD,KAKODS
      parameter   (KAKOD=4)
      dimension   KAKODS(KAKOD)
      common      /DWARF/ KAKODS
C     Continuum wavelength value type specification parameters.
C     (These parameters are packed and unpacked by "BET".)
      equivalence (KAKODS( 1),KAK1 )
      equivalence (KAKODS( 2),KAK2 )
      equivalence (KAKODS( 3),KAK3 )
C     !DASH
C     !EJECT
      external BORIS, BET, PICULET, MESHED, MASHED, HI, BYE
C
C               EMINTA(NW), LTYPE(NW), XLTIT(NW), XLB3(Li3len), EMU(L),
      dimension EMINTA(*),  LTYPE(*),  XLTIT(*),  XLB3(*),      EMU(*),
C
C               BRIGHT(NW,L), EMINT(NW,L), YSTAR(NW,L), BRIGHTA(NW),
     $          BRIGHT(*),    EMINT(*),    YSTAR(*),    BRIGHTA(*),
C
C               WAVES(NW), YSTARA(NW)
     $          WAVES(*),  YSTARA(*)
C
      call HI ('VOLVOX')
C     !BEG
      I = 0
  100 continue
        I = I+1
C
        if(I.le.NW) then
          call BET         (2, XLTIT(I))
          GOOD = (KAK2.eq.IU).and.(KAK3.eq.IL)
C          (Nothing more is needed for GOOD - this set of
C           continuum wavelengths comprises only the appropriate
C           subset of transition-related wavelengths.)
          if(GOOD) then
            call BORIS     (LTYPE(I), KAK1, KIND, IU, IL, WAVES(I),
     $                      XLTIT(I))
            if(LFB.eq.1) then
              call PICULET (I, NW, KIND, KTRN, L, LFB, EMU,
     $                      EMINT, YSTAR, BRIGHT,
     $                      XLB3(MMCIZ), XLB3(MMCIA), XLB3(MMBTI),
     $                      EMINTA, YSTARA, BRIGHTA,
     $                      XLB3(MMCZA), XLB3(MMCAA), XLB3(MMBTA))
            else if(LFB.eq.2) then
              call PICULET (I, NW, KIND, KTRN, L, LFB, EMU,
     $                      EMINT, YSTAR, BRIGHT,
     $                      XLB3(MMCZB), XLB3(MMCAB), XLB3(MMTIB),
     $                      EMINTA, YSTARA, BRIGHTA,
     $                      XLB3(MMZAB), XLB3(MMAAB), XLB3(MMTAB))
            end if
          end if
          go to 100
        end if
C
      if((IPEX.lt.0).or.(IPEX.eq.4)) then
        call MESHED        ('VOLVOX', 2)
        write (LUEO,101) I,NW,L,LTYPE(I),KAK2,KAK3,KAK1,WAVES(I)
  101   format(' ',3I8,I10,3I8,1PE22.12)
        call MASHED        ('VOLVOX')
      end if
C     !END
      call BYE ('VOLVOX')
C
      return
      end
