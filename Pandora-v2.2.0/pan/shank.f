      subroutine SHANK
     $(I,L,N,NW,TAU2,EMINT,MUX,KODE,YY,EMINTA,MYX,DIDH,DODIDH)
C
C     Rudolf Loeser, 1980 Jun 13
C---- Sets default Continuum Intensity data, for VISHNU.
C     !DASH
      save
C     !DASH
      real*8 DIDH, EMINT, EMINTA, TAU2, YY, ZERO
      integer I, J, KODE, L, MUX, MYX, N, NW
      logical DODIDH
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external ZERO1, HI, BYE
C
C               EMINT(Nmkuse,L), MUX(Nmkuse,L), DIDH(N,Nmkuse,L),
      dimension EMINT(NW,*),     MUX(NW,*),     DIDH(N,NW,*),
C
C               KODE(Nmkuse,L), YY(Nmkuse,L), MYX(Nmkuse,L),
     $          KODE(NW,*),     YY(NW,*),     MYX(NW,*),
C
C               EMINTA(Nmkuse)
     $          EMINTA(*)
C
      call HI ('SHANK')
C     !BEG
      do 100 J = 1,L
        EMINT(I,J) = -TAU2
        MUX(I,J)   = 0
        KODE(I,J)  = 3
        YY(I,J)    = ZERO
C
        if(DODIDH) then
          MYX(I,J) = 0
          call ZERO1 (DIDH(1,I,J),N)
        end if
C
        if(J.eq.1) then
          EMINTA(I) = ZERO
        end if
  100 continue
C     !END
      call BYE ('SHANK')
C
      return
      end
