      subroutine LYRA
     $(NL,ML,N,ITAU,CHI,ASTAR,SA,GM,CIJ,PIJ,A,C)
C
C     Rudolf Loeser, 2003 Mar 14
C---- Computes matrix a and vector c at depth itau, for CALYX.
C     !DASH
      save
C     !DASH
      real*8 A, ASTAR, C, CHI, CIJ, GM, PIJ, SA, SAV, SUM, Y, ZERO
      integer I, IJ, ILP, IP, ITAU, J, JIP, JP, L, ML, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external STYRAL, INDXIJ, STERNO, HI, BYE
C
C               CHI(N,NT), ASTAR(N,NT), GM(N,NSL), CIJ(N,NL**2), C(ML),
      dimension CHI(*),    ASTAR(*),    GM(N,*),   CIJ(N,*),     C(*),
C
C               PIJ(N,NL**2), A(ML,ML), SA(N,NT)
     $          PIJ(N,*),     A(ML,*),  SA(*)
C     !EJECT
C
      call HI ('LYRA')
C     !BEG
      do 102 I = 1,ML
        IP = I+1
C
        call STYRAL         (IP, 1, ITAU, ASTAR, CHI, Y)
        call INDXIJ         (1, IP, IJ)
        C(I) = GM(ITAU,1)*(CIJ(ITAU,IJ)+PIJ(ITAU,IJ)+Y)
C
        do 101 J = 1,ML
          JP = J+1
C
          if(J.lt.I) then
            call STYRAL     (IP, JP, ITAU, ASTAR, CHI, Y)
            call INDXIJ     (JP, IP, JIP)
            A(I,J) = -GM(ITAU,JP)*(CIJ(ITAU,JIP)+PIJ(ITAU,JIP)
     $                             +Y)
C
          else if(J.gt.I) then
            call INDXIJ     (JP, IP, JIP)
            call STERNO     (JP, IP, ITAU, SA, SAV)
            A(I,J) = -GM(ITAU,JP)*(CIJ(ITAU,JIP)+PIJ(ITAU,JIP)
     $                             -SAV)
C
          else
            SUM = ZERO
            do 100 L = 1,NL
              if(L.ne.IP) then
                call INDXIJ (IP, L, ILP)
                SUM = SUM+CIJ(ITAU,ILP)+PIJ(ITAU,ILP)
              end if
              if(L.le.I) then
                call STERNO (IP, L, ITAU, SA, SAV)
                SUM = SUM-SAV
              end if
              if(L.gt.IP) then
                call STYRAL (L, IP, ITAU, ASTAR, CHI, Y)
                SUM = SUM+Y
              end if
  100       continue
            A(I,J) = GM(ITAU,IP)*SUM
          end if
C
  101   continue
  102 continue
C     !END
      call BYE ('LYRA')
C
      return
      end
