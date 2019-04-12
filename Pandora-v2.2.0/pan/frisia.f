      subroutine FRISIA
     $(N,NL,XNU,XND,BDI,CIJ,CHT)
C
C     Rudolf Loeser, 1982 Feb 24
C---- Computes heating rates for bound-bound transitions.
C     !DASH
      save
C     !DASH
      real*8 BDI, BR, CHT, CIJ, FAC, ONE, T, XND, XNU
      integer I, IL, ILU, IU, IUL, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external RIGEL, INDXIJ, INDXUL, DIVIDE, HI, BYE
C
C               XND(N,NL), CIJ(N,NL**2), CHT(N,NT), BDI(N,NL), XNU(NSL)
      dimension XND(N,*),  CIJ(N,*),     CHT(N,*),  BDI(N,*),  XNU(*)
C
      call HI ('FRISIA')
C     !BEG
      call RIGEL        (13, FAC)
C
      do 102 IU = 2,NL
        do 101 IL = 1,(IU-1)
C
          T = FAC*(XNU(IU)-XNU(IL))
          call INDXUL   (IU, IL, IUL)
          call INDXIJ   (IL, IU, ILU)
C
          do 100 I = 1,N
            call DIVIDE (BDI(I,IU), BDI(I,IL), BR)
            CHT(I,IUL) = T*XND(I,IL)*CIJ(I,ILU)*(ONE-BR)
  100     continue
C
  101   continue
  102 continue
C     !END
      call BYE ('FRISIA')
C
      return
      end
