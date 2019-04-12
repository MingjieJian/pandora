      subroutine ALESIA
     $(N,NL,RKI,CKI,CQSI,XNE,SA,PLK,PKL,SPKL)
C
C     Rudolf Loeser, 1998 Feb 11
C---- Computes PLK (ionization rates) and PKL (recombination rates),
C     for diffusion calculation.
C     !DASH
      save
C     !DASH
      real*8 CKI, CQSI, FAC, PKL, PLK, RKI, SA, SPKL, SUM, TRM, XNE,
     $       ZERO
      integer I, J, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external ARRADD, HI, BYE
C
C               RKI(N,NSL), CKI(N,NSL), CQSI(N,NSL), XNE(N), PKL(N,NL),
      dimension RKI(*),     CKI(*),     CQSI(N,*),   XNE(*), PKL(N,*),
C
C               PLK(N,NL), SA(N), SPKL(N)
     $          PLK(*),    SA(*), SPKL(*)
C
      call HI ('ALESIA')
C     !BEG
C---- Compute PLK
      call ARRADD   (RKI,CKI,PLK,(N*NL))
C---- Compute PKL and its sum, SPKL
      do 101 I = 1,N
        FAC = XNE(I)*SA(I)
        SUM = ZERO
        do 100 J = 1,NL
          TRM = FAC*CQSI(I,J)
          SUM = SUM+TRM
          PKL(I,J) = TRM
  100   continue
        SPKL(I) = SUM
  101 continue
C     !END
      call BYE ('ALESIA')
C
      return
      end
