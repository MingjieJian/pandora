      subroutine FAZZ
     $(N,XKL,S,COP,CSF,XKT,EP,STZ,SL,LINE,VEC,ST)
C
C     Rudolf Loeser, 2006 Jan 24
C---- Computes ST, the total source function.
C     (This is version 4 of FAZZ.)
C     !DASH
      save
C     !DASH
      real*8 COP, CSF, DIV, EP, OEP, ONE, S, SL, ST, STZ, VEC, XCON,
     $       XDEN, XKL, XKT, XLIN, XNUM
      integer I, ICE, IPEX, LUEO, N
      character LINE*(*)
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
      equivalence (KZQ( 18),IPEX )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !EJECT
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
      equivalence (LINKDS( 4),ICE  )
C     !DASH
C     !EJECT
      external MOVE1, DIVIDE, ARRMUL, ARRADD, TROCK, MESHED, MASHED,
     $         HI, BYE
C
C               XKL(N), COP(N), CSF(N), XKT(N), ST(N), SL(N), STZ(N,2),
      dimension XKL(*), COP(*), CSF(*), XKT(*), ST(*), SL(*), STZ(N,*),
C
C               VEC(N), EP(N), S(N)
     $          VEC(*), EP(*), S(*)
C
      call HI ('FAZZ')
C     !BEG
C---- Set up SL
      if(ICE.eq.0) then
        LINE = 'SL = S'
        call MOVE1      (S, N, SL)
      else
        if(ICE.eq.2) then
C         STZ(i,1) = FRD, STZ(i,2) = GRD
C
          LINE = 'SL = [(1 + EP)*S + FRD] / [(1 + EP) + GRD]'
          do 100 I = 1,N
            OEP  = EP(I)+ONE
            XNUM = OEP*S(I)+STZ(I,1)
            XDEN = OEP+STZ(I,2)
            call DIVIDE (XNUM, XDEN, SL(I))
  100     continue
        else
C         STZ(i,1) = RXI, STZ(i,2) = JNU
C
          LINE = 'SL = S + RXI*JNU'
          call ARRMUL   (STZ(I,1), STZ(I,2), SL, N)
          call ARRADD   (SL, S, SL, N)
        end if
      end if
C
C---- Now compute ST
      call TROCK        (N, XKL, SL, COP, CSF, XKT, ST, VEC)
C
C---- Dump?
      if((IPEX.lt.0).or.(IPEX.eq.27)) then
        call MESHED     ('FAZZ', 2)
        write (LUEO,101) N,ICE,LINE
  101   format(' ','For ST:  N =',I5,', ICE =',I2,5X,A//
     $         ' ',12X,'XKL',7X,'COP',7X,'CSF',7X,'XKT',6X,'STZ1',
     $             6X,'STZ2',10X,'EP',11X,'S',10X,'SL',10X,'ST')
        write (LUEO,102) (I,XKL(I),COP(I),CSF(I),XKT(I),STZ(I,1),
     $                    STZ(I,2),EP(I),S(I),SL(I),ST(I),I=1,N)
  102   format(5(' ',I5,1P10E12.4/))
        call MASHED     ('FAZZ')
      end if
C     !END
      call BYE ('FAZZ')
C
      return
      end
