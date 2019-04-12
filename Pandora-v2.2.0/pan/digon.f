      subroutine DIGON
     $(NO,N,MSFT,S,SD,KSE,KSEDA,VEC)
C
C     Rudolf Loeser, 1999 Dec 29
C---- Compares source functions computed with and without diffusion.
C     (This is version 5 of DIGON.)
C     !DASH
      save
C     !DASH
      real*8 S, SD, VEC
      integer KSE, KSEDA, MSFT, N, NO
C     !DASH
      external JANET, HI, BYE
C
C               VEC(N), SD(N), S(N)
      dimension VEC(*), SD(*), S(*)
C
      call HI ('DIGON')
C     !BEG
      if(NO.gt.0) then
        if((MSFT.eq.0).or.(MSFT.eq.1)) then
          if((KSEDA.eq.2).and.(KSE.eq.2)) then
            call JANET  (NO, N, S, SD, VEC)
          end if
        end if
      end if
C     !END
      call BYE ('DIGON')
C
      return
      end
