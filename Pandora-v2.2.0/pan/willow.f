      subroutine WILLOW
     $(F,NF,T,NT,Z,M)
C
C     Rudolf Loeser, 1968 May 29
C---- Computes the two vectors Z and M, each of length NT, that
C     represent the matrix producing a transformation from the set
C     of coordinate points F (length NF) to the set of coordinate
C     points T (length NT).
C     !DASH
      save
C     !DASH
      real*8 F, ONE, T, X, Y, Z
      integer I, IM, J, K, M, NF, NT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external  DIVIDE, SETI, SET1, HI, BYE
C
C               F(NF), T(NT), Z(NT), M(NT)
      dimension F(*),  T(*),  Z(*),  M(*)
C     !EJECT
C
      call HI ('WILLOW')
C     !BEG
      J = 0
      do 101 I = 1,NT
        K = I
        Y = T(I)
C
  100   continue
          J = J+1
          X = F(J)
          if(J.gt.NF) then
            IM = NT-K+1
            call SETI     (M(K),1,IM,M(K-1))
            call SET1     (Z(K),  IM,Z(K-1))
            goto 102
          else if(J.eq.NF) then
            M(I) = J-1
            call DIVIDE   ((X-Y),(X-F(J-1)),Z(I))
          else
            if(Y.gt.X) then
              goto 100
C
            else if(Y.eq.X) then
              M(I) = J
              Z(I) = ONE
            else
              M(I) = J-1
              call DIVIDE ((X-Y),(X-F(J-1)),Z(I))
            end if
          end if
          J = J-1
        continue
C
  101 continue
C
  102 continue
C     !END
      call BYE ('WILLOW')
C
      return
      end
