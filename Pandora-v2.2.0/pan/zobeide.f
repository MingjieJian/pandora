      subroutine ZOBEIDE
     $(DL,K,ID)
C
C     Rudolf Loeser, 1992 Mar 24
C---- Gets DL-indices, for YING.
C     (This is version 3 of ZOBEIDE.)
C     !DASH
      save
C     !DASH
      real*8 DL, ONE, ZERO
      integer ID, INDEX, K
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  QUEBEC, HI, BYE
      intrinsic abs, sign
C
C               DL(K), ID(5)
      dimension DL(*), ID(*)
C
      call HI ('ZOBEIDE')
C     !BEG
      if(sign(DL(1),ONE).ne.sign(DL(K),ONE)) then
        call QUEBEC (DL,K,'DL','ZOBEIDE',INDEX)
        if(abs(INDEX-(K/2)).gt.2) then
          ID(3) = INDEX
        else
          ID(3) = (3*K)/7
        end if
      else
        ID(3) = (2*K)/7
      end if
C
      ID(1) = 1
      ID(5) = K
      ID(2) = (ID(3)+ID(1))/2
      ID(4) = (ID(3)+ID(5))/2
C     !END
      call BYE ('ZOBEIDE')
C
      return
      end
