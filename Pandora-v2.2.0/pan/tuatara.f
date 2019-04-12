      subroutine TUATARA
     $(TE,HN1,HNK,HE1N1,HE2N1,XNE,N,XLHE)
C
C     Rudolf Loeser, 1981 Feb 02
C---- Computes Helium contribution to
C     translational thermal conductivity, as given by
C     Nowak and Ulmschneider, Astron.Astrophys. Vol 60, 413 (1977).
C     !DASH
      save
C     !DASH
      real*8 A, B, C, CROSS, D, HE1N1, HE2N1, HN1, HNK, RAT, RT, TE,
     $       XLHE, XNE, ZERO
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external ZERO1, DIVIDE, COULOMB, HI, BYE
C
C               HN1(N), HNK(N), HE1N1(N), HE2N1(N), XNE(N), XLHE(N),
      dimension HN1(*), HNK(*), HE1N1(*), HE2N1(*), XNE(*), XLHE(*),
C
C               TE(N)
     $          TE(*)
C
      data A,B,C,D /1.0486D-17, 1.2305D0, 1.3539D0, 7.23D-2/
C
      call HI ('TUATARA')
C     !BEG
      call ZERO1       (XLHE,N)
C
      do 100 I = 1,N
        if(XNE(I).ne.ZERO) then
          call COULOMB ('HE  ', 'E   ', TE(I), XNE(I), CROSS)
          XLHE(I) = XLHE(I)+D*CROSS*XNE(I)
        end if
  100 continue
C
      do 101 I = 1,N
        if(HNK(I).ne.ZERO) then
          call COULOMB ('HE  ', 'H+  ', TE(I), XNE(I), CROSS)
          XLHE(I) = XLHE(I)+C*CROSS*HNK(I)
        end if
  101 continue
C
      do 102 I = 1,N
        if(HN1(I).ne.ZERO) then
          call COULOMB ('HE  ', 'H   ', TE(I), XNE(I), CROSS)
          XLHE(I) = XLHE(I)+B*CROSS*HN1(I)
        end if
  102 continue
C
      do 103 I = 1,N
        if(HE2N1(I).ne.ZERO) then
          call COULOMB ('HE  ', 'HE+ ', TE(I), XNE(I), CROSS)
          XLHE(I) = XLHE(I)+CROSS*HE2N1(I)
        end if
  103 continue
C
      do 104 I = 1,N
        if((XLHE(I).ne.ZERO).and.(HE1N1(I).ne.ZERO)) then
          call COULOMB ('HE  ', 'HE  ', TE(I), XNE(I), CROSS)
          call DIVIDE  (XLHE(I), HE1N1(I), RAT)
          XLHE(I) = CROSS+RAT
        end if
        if(XLHE(I).ne.ZERO) then
          RT = sqrt(TE(I))
          call DIVIDE  ((A*RT), XLHE(I), XLHE(I))
        end if
  104 continue
C     !END
      call BYE ('TUATARA')
C
      return
      end
