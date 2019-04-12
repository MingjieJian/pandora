      subroutine DEEHEE
     $(TE,PART,ENE,CHAR,ATNO,AMASS,ABUN,XK,VCM,W,IW,DUMP,DEE)
C
C     Rudolf Loeser, 1990 Jul 05
C---- Computes diffusion coefficients.
C     Adapted from DHEL, written by
C
C     J u a n   F o n t e n l a .
C
C---- TE      - temperature
C     PART(1) - Hydrogen partial pressure
C     PART(2) - Protons partial pressure
C     PART(3) - HeI partial pressure
C     PART(4) - HeII partial pressure
C     PART(5) - HeIII partial pressure
C     ENE     - electron density
C     CHAR    - ion charge
C     ATNO    - atomic number
C     AMASS   - atomic mass
C     ABUN    - abundance
C     XK      - k, Boltzmann constant
C     VCM     - 2*k/mass(Hydrogen)
C
C     W       - scratch storage
C     IW      - scratch storage
C     LU      - logical unit number for debug output
C
C     DEE     - array of diffusion coefficients
C     !DASH
      save
C     !DASH
      real*8 A4, ABUN, ALFX, ALFY, ALFZ, AMASS, AMT, ATNO, CHAR, CNOS,
     $       CX10, CY10, CZ10, D01, D01T, DEE, EMT, ENE, PART, PHEL,
     $       PHYD, T4, TE, TNTHSND, VCM, W, XK, ZERO
      integer I, IW, J, LUEO
      logical DUMP
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external DFHYCOF, DFHECOL, DFOCOF, DFAMAT, DFEMAT, LINER, HI, BYE
C
      dimension W(*), IW(*)
C
C               DEE(4,5), PART(5)
      dimension DEE(4,*), PART(*)
C
      dimension AMT(3,3), EMT(3,5), CNOS(5,5)
C
      data ZERO,TNTHSND /0.D0, 1.D4/
C
      call HI ('DEEHEE')
C     !BEG
      T4 = TE/TNTHSND
      A4 = log10(T4)
C
C---- Hydrogen
      PHYD = PART(1)+PART(2)
      call DFHYCOF (TE, PART, D01, D01T, DUMP)
      DEE(1,1) = D01
      DEE(1,2) = ZERO
      DEE(1,3) = ZERO
      DEE(1,4) = ZERO
      DEE(1,5) = D01T
C
C---- Helium
      PHEL = PART(3)+PART(4)+PART(5)
      call DFHECOL (CNOS, TE, T4, A4, PART, ENE, AMASS, XK, DUMP)
      call DFAMAT  (AMT, CNOS, PART, PHEL, W, IW, DUMP)
      call DFOCOF  (CNOS, AMASS, T4, A4, ALFX, ALFY, ALFZ, CX10, CY10,
     $              CZ10, DUMP)
      call DFEMAT  (EMT, CNOS, PART, VCM, TE, AMASS, PHYD, PHEL, D01,
     $              D01T, ALFX, ALFY, ALFZ, CX10, CY10, CZ10, DUMP)
C
      do 101 I = 2,4
        do 100 J = 1,5
          DEE(I,J) =  AMT(I-1,1)*EMT(1,J)+AMT(I-1,2)*EMT(2,J)
     $               +AMT(I-1,3)*EMT(3,J)
  100   continue
  101 continue
C
      if(DUMP) then
        call LINER (1, LUEO)
        write (LUEO,102)
  102   format(' ','Matrix of d-coefficients, as computed')
        write (LUEO,103) (I,(DEE(I,J),J=1,5),I=1,4)
  103   format(' ',I5,1P5E11.3)
      end if
C     !END
      call BYE ('DEEHEE')
C
      return
      end
