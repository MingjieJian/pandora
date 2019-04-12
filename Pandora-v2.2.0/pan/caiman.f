      subroutine CAIMAN
     $(TE,HN1,HNK,HE1N1,HE2N1,HE2NK,XNE,N,XLE)
C
C     Rudolf Loeser, 1981 Feb 02
C---- Computes electron constribution to
C     translational thermal conductivity, as given by
C     Nowak and Ulmschneider, Astron.Astrophys. Vol 60, 413 (1977).
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, HE1N1, HE2N1, HE2NK, HN1, HNK, R, RAT, RT, TE,
     $       XLE, XNE, ZERO
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
      external ZERO1, COULOMB, DIVIDE, HI, BYE
C
C               HE2NK(N), HN1(N), HNK(N), HE1N1(N), HE2N1(N), XNE(N),
      dimension HE2NK(*), HN1(*), HNK(*), HE1N1(*), HE2N1(*), XNE(*),
C
C               XLE(N), TE(N)
     $          XLE(*), TE(*)
C
      data A,B,C,D /6.1159D-11, 4.838D-1, 2.478D-1, 2.46D-1/
C
      call HI ('CAIMAN')
C     !BEG
      call ZERO1       (XLE, N)
C
      do 100 I = 1,N
        if(HN1(I).ne.ZERO) then
          call COULOMB ('E   ', 'H   ', TE(I), XNE(I), R)
          XLE(I) = XLE(I)+R*HN1(I)
        end if
  100 continue
C
      do 101 I = 1,N
        if(HE1N1(I).ne.ZERO) then
          call COULOMB ('E   ', 'HE  ', TE(I), XNE(I), R)
          XLE(I) = XLE(I)+R*HE1N1(I)
        end if
  101 continue
C
      do 102 I = 1,N
        if(HE2NK(I).ne.ZERO) then
          call COULOMB ('E   ', 'HE++', TE(I), XNE(I), R)
          XLE(I) = XLE(I)+D*R*HE2NK(I)
        end if
  102 continue
C     !EJECT
      do 103 I = 1,N
        if(HE2N1(I).ne.ZERO) then
          call COULOMB ('E   ', 'HE+ ', TE(I), XNE(I), R)
          XLE(I) = XLE(I)+C*R*HE2N1(I)
        end if
  103 continue
C
      do 104 I = 1,N
        if(HNK(I).ne.ZERO) then
          call COULOMB ('E   ', 'H+  ', TE(I), XNE(I), R)
          XLE(I) = XLE(I)+B*R*HNK(I)
        end if
  104 continue
C
      do 105 I = 1,N
        call COULOMB   ('E   ', 'E   ', TE(I), XNE(I), R)
        call DIVIDE    (XLE(I), XNE(I), RAT)
        XLE(I) = R+RAT
        RT     = sqrt(TE(I))
        call DIVIDE    ((A*RT), XLE(I), XLE(I))
  105 continue
C     !END
      call BYE ('CAIMAN')
C
      return
      end
