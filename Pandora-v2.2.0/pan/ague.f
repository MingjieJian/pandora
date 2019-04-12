      subroutine AGUE
     $(N,FRS,HND,NMLR,V,VREQ,HREQ)
C
C     Rudolf Loeser, 2005 Sep 15
C---- Computes auxiliary data for mass loss rates.
C     !DASH
      save
C     !DASH
      real*8 DR, FRS, HND, HR, HREQ, V, VR, VREQ, ZERO
      integer I, N, NMLR
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external HI, BYE
C
C               FRS(N), HND(N), V(N), VREQ(N), HREQ(N)
      dimension FRS(*), HND(*), V(*), VREQ(*), HREQ(*)
C
      call HI ('AGUE')
C     !BEG
      do 100 I = 1,N
        DR = ZERO
        if(FRS(I).ne.ZERO) then
          DR = FRS(NMLR)/FRS(I)
        end if
        HR = ZERO
        if(HND(I).ne.ZERO) then
          HR = HND(NMLR)/HND(I)
        end if
        VR = ZERO
        if(V(I).ne.ZERO) then
          VR = V(NMLR)/V(I)
        end if
C
        DR = DR**2
        VREQ(I) = V(NMLR)*DR*HR
        HREQ(I) = HND(NMLR)*DR*VR
  100 continue
C     !END
      call BYE ('AGUE')
C
      return
      end
