      subroutine MALLOW
     $(TE,N,NLH,LIMP,HNI,A,B,E,F)
C
C     Rudolf Loeser, 1981 May 29
C---- Computes default Hydrogen populations for levels below LIMP.
C     !DASH
      save
C     !DASH
      real*8 A, B, CON58, E, F, H, HL, HN, HNI, SE, SF, TE, ZERO
      integer I, J, LIMP, N, NLH
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external DIVIDE, RIGEL, HI, BYE
C
C               TE(N), HNI(N,LIMP), A(LIMP), B(LIMP), E(LIMP), F(LIMP)
      dimension TE(*), HNI(N,*),    A(*),    B(*),    E(*),    F(*)
C
      call HI ('MALLOW')
C     !BEG
      call RIGEL      (58, CON58)
C
      do 101 I = 1,N
        call DIVIDE   (CON58, TE(I), H)
        HN = HNI(I,NLH)
        HL = HNI(I,LIMP)
C
        do 100 J = (NLH+1),(LIMP-1)
C
          if(HN.eq.ZERO) then
            SE = ZERO
          else
            SE = exp(-(H*E(J)))
          end if
          if(HL.eq.ZERO) then
            SF = ZERO
          else
            SF = exp(-(H*F(J)))
          end if
C
          HNI(I,J) = A(J)*HN*SE+B(J)*HL*SF
  100   continue
C
  101 continue
C     !END
      call BYE ('MALLOW')
C
      return
      end
