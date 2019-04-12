      subroutine BOA
     $(TE,HN1,HNK,HE1N1,XNE,N,XLH)
C
C     Rudolf Loeser, 1981 Feb 02
C---- Computes Hydrogen contribution to translational thermal
C     conductivity, as given by
C     Nowak and Ulmschneider, Astron.Astrophys. Vol 60, 413 (1977).
C     !DASH
      save
C     !DASH
      real*8 A, B, C, HE1N1, HN1, HNK, R, RAT, RT, TE, XLH, XNE, ZERO
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
C               TE(N), HN1(N), HNK(N), HE1N1(N), XNE(N), XLH(N)
      dimension TE(*), HN1(*), HNK(*), HE1N1(*), XNE(*), XLH(*)
C
      data A,B,C /2.4107D-12, 8.5D-1, 1.2305D0/
C
      call HI ('BOA')
C     !BEG
      call ZERO1       (XLH, N)
C
      do 100 I = 1,N
        if(HE1N1(I).ne.ZERO) then
          call COULOMB ('H   ', 'HE  ', TE(I), XNE(I), R)
          XLH(I) = XLH(I)+C*R*HE1N1(I)
        end if
  100 continue
C
      do 101 I = 1,N
        if(XNE(I).ne.ZERO) then
          call COULOMB ('H   ', 'E   ', TE(I), XNE(I), R)
          XLH(I) = XLH(I)+R*XNE(I)
        end if
  101 continue
C
      do 102 I = 1,N
        if(HNK(I).ne.ZERO) then
          call COULOMB ('H   ', 'H+  ', TE(I), XNE(I), R)
          XLH(I) = XLH(I)+B*R*HNK(I)
        end if
  102 continue
C
      do 103 I = 1,N
        call COULOMB   ('H   ', 'H   ', TE(I), XNE(I), R)
        RT = sqrt(TE(I))
        call DIVIDE    (XLH(I), HN1(I), RAT)
        XLH(I) = R+RAT
        call DIVIDE    ((A*RT), XLH(I), XLH(I))
  103 continue
C     !END
      call BYE ('BOA')
C
      return
      end
