      subroutine OBERON
     $(N,BDIJ,INDX,NRAD,DLOG,SIG)
C
C     Rudolf Loeser, 1980 Oct 30
C---- Sets up logs of ratios of departure coefficients, for PLINK.
C     (This is version 4 of OBERON.)
C     !DASH
      save
C     !DASH
      real*8 B, BDIJ, DLOG, SIG, ZERO
      integer I, IL, INDX, IU, J, N, NRAD
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external BRAT, HI, BYE
C
C               DLOG(N,NRAD), BDIJ(N,NL), INDX(NRAD)
      dimension DLOG(N,*),    BDIJ(*),    INDX(*)
C
      call HI ('OBERON')
C     !BEG
      do 101 J = 1,NRAD
C
        IU = INDX(J)/100
        IL = INDX(J)-100*IU
C
        do 100 I = 1,N
          call BRAT (I, IU, IL, BDIJ, B)
          if(B.le.ZERO) then
            DLOG(I,J) = SIG
          else
            DLOG(I,J) = log10(B)
          end if
  100   continue
  101 continue
C     !END
      call BYE ('OBERON')
C
      return
      end
