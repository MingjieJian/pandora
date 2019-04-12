      subroutine ECHUIR
     $(N,IU,IL,NL,AIJ,AMASS,TE,XNC,HN1,FC,A)
C
C     Rudolf Loeser, 1991 May 15
C---- Computes FC, for DERVENI.
C     !DASH
      save
C     !DASH
      real*8 A, AIJ, AMASS, F, FAC, FC, HN1, RT, TE, XNC, ZERO
      integer I, IL, IU, N, NL
      character QELSM*8
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
      equivalence (QZQ(  2),QELSM)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external KERBAU, HI, BYE
C
C               AIJ(NL,NL), TE(N), FC(N), HN1(N), XNC(N)
      dimension AIJ(NL,*),  TE(*), FC(*), HN1(*), XNC(*)
C
      data FAC /9.77D-25/
C
      call HI ('ECHUIR')
C     !BEG
      A = AIJ(IU,IL)
C
      if(A.le.ZERO) then
        if((QELSM(:3).eq.'H  ').and.(IL.eq.1)) then
          call KERBAU (IU, IL, ZERO, ZERO, A)
        end if
      end if
C
      if(A.gt.ZERO) then
        F = A*FAC
        do 101 I = 1,N
          RT    = sqrt(TE(I)/AMASS)
          FC(I) = F*RT*HN1(I)
  101   continue
      end if
C     !END
      call BYE ('ECHUIR')
C
      return
      end
