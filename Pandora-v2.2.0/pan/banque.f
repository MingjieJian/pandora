      subroutine BANQUE
     $(CEK,N,NLM,LU)
C
C     Rudolf Loeser, 1980 Mar 14
C---- Prints the CHECKs.
C     (This is version 2 of BANQUE.)
C     !DASH
      save
C     !DASH
      real*8 CEK
      integer J, LU, N, NLM
C     !DASH
      external JOSEPH, LINER, BANOIC, HI, BYE
C
C               CEK(N,NLM)
      dimension CEK(N,*)
C
      call HI ('BANQUE')
C     !BEG
      if((LU.gt.0).and.(NLM.gt.0)) then
        call JOSEPH   (LU)
        write (LU,100)
  100   format(' ','Consistency CHECKs ',
     $             'from ratios of departure coefficients')
C
        do 101 J = 1,NLM
          call BANOIC (N, (J+2), CEK(1,J), LU)
  101   continue
C
        call LINER    (2, LU)
        write (LU,102)
  102   format(' ','CHECKs are calculated using: RBDS for radiative ',
     $             'transitions, and RBD* for all other transitions.')
        call JOSEPH   (LU)
      end if
C     !END
      call BYE ('BANQUE')
C
      return
      end
