      subroutine REDBUD
     $(G,FI,WN,IMAX,N)
C
C     Rudolf Loeser, 1968 May 28
C---- Multiplies the matrices G and FI, to form the QR weight matrix.
C     Note that FI(i,j)=0 when i .gt. j+1.
C     Note that rows of G(i,j)=0 for i .gt. IMAX.
C---- Furthermore, for each WN(i,j) separate sums of positive and
C     negative contributionsi are accumulated, and WN(i,j) is set =0
C     if the two contributions nearly cancel.
C     !DASH
      save
C     !DASH
      real*8 CRIT, DS, FI, G, P, S, SM, SP, WN, ZERO
      integer I, IMAX, J, JP, K, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  DIVIDE, HI, BYE
      intrinsic abs, min
C
C               G(N,N), FI(N,N), WN(N,N)
      dimension G(N,*), FI(N,*), WN(N,*)
C
      data CRIT /2.D-12/
C     !EJECT
C
      call HI ('REDBUD')
C     !BEG
      do 102 J = 1,N
        JP  = min((J+1),N)
        do 101 I = 1,N
          if(I.le.IMAX) then
            SP = ZERO
            SM = ZERO
            do 100 K = 1,JP
              P = G(I,K)*FI(K,J)
              if(P.gt.ZERO) then
                SP = SP+P
              else if(P.lt.ZERO) then
                SM = SM+P
              end if
  100       continue
            S = SP+SM
            if(S.ne.ZERO) then
              call DIVIDE (S,(SP-SM),DS)
              if(abs(DS).le.CRIT) then
                S = ZERO
              end if
            end if
            WN(I,J) = S
          else
            WN(I,J) = ZERO
          end if
  101   continue
  102 continue
C     !END
      call BYE ('REDBUD')
C
      return
      end
